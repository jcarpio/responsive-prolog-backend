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

// Extrae todas las variables únicas de una consulta Prolog
function detectAllVars(query) {
  const matches = query.match(/\b([A-Z_][A-Za-z0-9_]*)\b/g);
  return matches ? [...new Set(matches)] : []; // Elimina duplicados y devuelve solo variables
}

app.post('/run', (req, res) => {
  const { facts = '', query } = req.body;

  if (!query) {
    return res.status(400).json({ error: 'No Prolog query provided' });
  }

  const vars = detectAllVars(query);

  const wrappedCode = `
:- use_module(library(clpfd)).

${facts}

main :- 
    ( ${cleanQuery(query)} -> 
        forall(${cleanQuery(query)}, (format("${vars.map(v => `${v} = ~w`).join(', ')}\\n", [${vars.join(', ')}])))
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

    return res.json({ output: stdout.trim() || "false." });
  });
});

app.get('/', (req, res) => {
  res.send('✅ Responsive Prolog Backend is running.');
});

app.listen(PORT, () => {
  console.log(`Server is running on port ${PORT}`);
});
