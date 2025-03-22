import express from 'express';
import cors from 'cors';
import { exec } from 'child_process';
import fs from 'fs';
import path from 'path';

const app = express();
const PORT = process.env.PORT || 3000;

app.use(cors());
app.use(express.json());

app.post('/run', (req, res) => {
  const { facts = '', query } = req.body;

  if (!query) {
    return res.status(400).json({ error: 'No Prolog query provided' });
  }

  const wrappedCode = `
${facts}

main :- (${query}), writeln(${detectMainVar(query)}), fail.
:- main, halt.
`;

  const filePath = path.join('/tmp', `query_${Date.now()}.pl`);
  fs.writeFileSync(filePath, wrappedCode);

  exec(`swipl -q -f ${filePath}`, { timeout: 5000 }, (err, stdout, stderr) => {
    fs.unlinkSync(filePath);
    if (err) {
      return res.status(500).json({ error: stderr || err.message });
    }
    return res.json({ output: stdout });
  });
});

app.get('/', (req, res) => {
  res.send('Responsive Prolog Backend is running.');
});

app.listen(PORT, () => {
  console.log(`Server is running on port ${PORT}`);
});

function detectMainVar(code) {
  const match = code.match(/\b([A-Z_][A-Za-z0-9_]*)\b/);
  return match ? match[1] : '"✅ Consulta sin variables."';
}
