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

app.post('/run', (req, res) => {
  const { facts = '', query } = req.body;

  if (!query) {
    return res.status(400).json({ error: 'No Prolog query provided' });
  }

  const cleanedQuery = cleanQuery(query);

  const wrappedCode = `
:- use_module(library(clpfd)).

${facts}

main :-
    catch(run_query, E, (writeln('⚠️ Error en la consulta:'), writeln(E))),
    halt.

run_query :-
    Query = (${cleanedQuery}),
    copy_term(Query, Copy, Bindings),
    call(Copy),
    print_bindings(Bindings),
    fail.
run_query.

print_bindings([]).
print_bindings([Name=Value|Rest]) :-
    format("~w = ~w", [Name, Value]),
    (Rest \= [] -> write(', ') ; true),
    nl,
    print_bindings(Rest).
`;

  const filePath = path.join('/tmp', `query_${Date.now()}.pl`);
  fs.writeFileSync(filePath, wrappedCode);

  exec(`swipl -q -f ${filePath}`, { timeout: 5000 }, (err, stdout, stderr) => {
    fs.unlinkSync(filePath);

    const goalFailedPattern = /Goal \(directive\) failed: user:\(main,halt\)/;

    if (goalFailedPattern.test(stderr)) {
      return res.json({ output: "false." });
    }

    if (err && !stdout.trim()) {
      return res.status(500).json({ error: stderr || err.message });
    }

    if (!stdout.trim()) {
      return res.json({ output: "⚠️ La consulta no produjo ningún resultado." });
    }

    return res.json({ output: stdout.trim() });
  });
});

app.get('/', (req, res) => {
  res.send('✅ Responsive Prolog Backend is running.');
});

app.listen(PORT, () => {
  console.log(`Server is running on port ${PORT}`);
});
