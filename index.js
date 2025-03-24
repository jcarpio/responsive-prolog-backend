import express from 'express';
import cors from 'cors';
import { exec } from 'child_process';
import fs from 'fs';
import path from 'path';

const app = express();
const PORT = process.env.PORT || 3000;

app.use(cors());
app.use(express.json());

function cleanQuery(q) {
  return q.trim().replace(/\.$/, '');
}

// Extrae todas las variables de una consulta Prolog
function detectAllVars(query) {
  const matches = query.match(/\b([A-Z_][A-Za-z0-9_]*)\b/g);
  return matches ? [...new Set(matches)] : ['true']; // Elimina duplicados y usa 'true' si no hay variables
}

// Nueva función para formatear la salida al estilo de SWI-Prolog
function formatPrologOutput(stdout, vars) {
  // Si la salida es 'false', devolver 'false.'
  if (stdout.trim() === 'false') return 'false.';

  // Divide la salida en líneas
  const lines = stdout.trim().split('\n');

  // Formatea cada línea para que coincida con el formato de SWI-Prolog
  const formattedLines = lines.map(line => {
    // Divide la línea en valores separados por comas
    const values = line.split(',').map(val => val.trim());

    // Combina variables con sus valores
    return vars.map((v, index) => `${v}=${values[index]}`).join('\n');
  });

  return formattedLines.join('\n');
}

app.post('/run', (req, res) => {
  const { facts = '', query } = req.body;

  if (!query) {
    return res.status(400).json({ error: 'No Prolog query provided' });
  }

  const vars = detectAllVars(query); // Lista de variables
  const varsStr = vars.join(', '); // Variables para el formato de salida

  const wrappedCode = `
:- use_module(library(clpfd)).

${facts}

main :- 
    ( ${cleanQuery(query)} -> 
        forall(${cleanQuery(query)}, (writeln((${varsStr}))))
    ; 
        writeln(false)
    ).
:- main, halt.
`;

  const filePath = path.join('/tmp', `query_${Date.now()}.pl`);
  fs.writeFileSync(filePath, wrappedCode);

  exec(`swipl -q -f ${filePath}`, { timeout: 5000 }, (err, stdout, stderr) => {
    fs.unlinkSync(filePath);

    if (err && !stdout.trim()) {
      return res.status(500).json({ error: stderr || err.message });
    }

    // Formatear la salida
    const formattedOutput = formatPrologOutput(stdout, vars);

    return res.json({ output: formattedOutput });
  });
});

app.get('/', (req, res) => {
  res.send('✅ Responsive Prolog Backend is running.');
});

app.listen(PORT, () => {
  console.log(`Servidor corriendo en puerto ${PORT}`);
});
