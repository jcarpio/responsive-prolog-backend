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
        findall((${vars.join(',')}), (${cleanQueryStr}), Solutions),
        (Solutions \= [] ->
            maplist(writeln, Solutions)
        ;
            writeln(false))
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

    const output = stdout.trim() === 'false' ? 'false.' : stdout.trim();
    return res.json({ output });
  });
});

app.get('/', (req, res) => {
  res.send('✅ Responsive Prolog Backend is running.');
});

app.listen(PORT, () => {
  console.log(`Servidor corriendo en puerto ${PORT}`);
});
