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
  const matches = query.match(/\b([A-Z][A-Za-z0-9_]*)\b/g);
  return matches ? [...new Set(matches)] : ['true']; // Elimina duplicados y usa 'true' si no hay variables
}

app.post('/run', (req, res) => {
  const { facts = '', query } = req.body;
  
  if (!query) {
    return res.status(400).json({ error: 'No Prolog query provided' });
  }
  
  const vars = detectAllVars(query);
  const cleanQueryStr = cleanQuery(query);
  
  const wrappedCode = `
:- use_module(library(clpfd)).

${facts}

main :-
    (${cleanQueryStr} ->
        writeln('true'),
        writeln((${vars.join(', ')}))
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
    
    // Procesar la salida para que coincida con el formato deseado
    const lines = stdout.trim().split('\n');
    let output = 'false.';
    
    if (lines[0] === 'true') {
      // Formatear cada valor con su variable correspondiente
      output = vars.map((variable, index) => `${variable}=${lines[1].split(',')[index].trim()}`).join('\n');
    }
    
    return res.json({ output });
  });
});

app.get('/', (req, res) => {
  res.send('✅ Responsive Prolog Backend is running.');
});

app.listen(PORT, () => {
  console.log(`Servidor corriendo en puerto ${PORT}`);
});
