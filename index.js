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

function detectMainVar(code) {
  const match = code.match(/\b([A-Z_][A-Za-z0-9_]*)\b/);
  return match ? match[1] : 'true'; // Default para writeln
}

app.post('/run', (req, res) => {
  const { facts = '', query } = req.body;

  if (!query) {
    return res.status(400).json({ error: 'No Prolog query provided' });
  }

  const wrappedCode = `
:- use_module(library(clpfd)).

${facts}

main :- catch( ( ${cleanQuery(query)}, writeln(${detectMainVar(query)}), fail ), E, (writeln(E)) ).
:- main, halt.
`;

  const filePath = path.join('/tmp', `query_${Date.now()}.pl`);
  fs.writeFileSync(filePath, wrappedCode);

  exec(`swipl -q -f ${filePath}`, { timeout: 5000 }, (err, stdout, stderr) => {
    fs.unlinkSync(filePath);

    if (err && !stdout.trim()) {
      return res.status(500).json({ error: stderr || err.message });
    }

    if (!stdout.trim()) {
      return res.json({ output: "⚠️ La consulta no produjo ningún resultado." });
    }

    return res.json({ output: stdout });
  });
});

app.get('/', (req, res) => {
  res.send('✅ Responsive Prolog Backend is running.');
});

app.listen(PORT, () => {
  console.log(`Server is running on port ${PORT}`);
});
