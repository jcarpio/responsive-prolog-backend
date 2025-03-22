// responsive-prolog-backend
// API simple en Node.js para ejecutar código Prolog

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
  const { code } = req.body;
  if (!code) return res.status(400).json({ error: 'No Prolog code provided' });

  // Guardamos el código en un archivo temporal
  const filePath = path.join('/tmp', `query_${Date.now()}.pl`);
  const fullCode = `${code}\n:- halt.`;
  fs.writeFileSync(filePath, fullCode);

  // Ejecutamos SWI-Prolog con el archivo
  exec(`swipl -q -f ${filePath}`, (err, stdout, stderr) => {
    fs.unlinkSync(filePath); // Borramos el archivo temporal
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
