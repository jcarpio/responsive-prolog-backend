import { useState } from "react";
import 'bootstrap/dist/css/bootstrap.min.css';

export default function PrologMobile() {
  const [facts, setFacts] = useState(`padre(juan, maria).
padre(juan, pedro).
padre(pedro, luis).
abuelo(X, Y) :- padre(X, Z), padre(Z, Y).`);
  const [query, setQuery] = useState("abuelo(X, luis).");
  const [output, setOutput] = useState("");

  const handleRun = async () => {
    setOutput("⏳ Ejecutando...");

    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), 5000); // 5 segundos

    try {
      const response = await fetch("https://responsive-prolog-backend.onrender.com/run", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ facts, query }),
        signal: controller.signal
      });

      clearTimeout(timeoutId);

      const data = await response.json();
      if (data.output) setOutput(data.output);
      else setOutput("⚠️ Error: " + (data.error || "Desconocido"));
    } catch (err) {
      if (err.name === 'AbortError') {
        setOutput("⏱️ Tiempo de espera agotado (5s). La consulta tardó demasiado.");
      } else {
        setOutput("⚠️ Error de red: " + err.message);
      }
    }
  };

  return (
    <div className="container py-4">
      <h1 className="text-center mb-4">🧠 Prolog Online</h1>

      <div className="mb-4">
        <label className="form-label fw-bold">🔧 Base de conocimiento:</label>
        <textarea
          className="form-control"
          style={{ height: '200px' }}
          value={facts}
          onChange={(e) => setFacts(e.target.value)}
        />
      </div>

      <div className="mb-4">
        <label className="form-label fw-bold">❓ Consulta:</label>
        <textarea
          className="form-control"
          style={{ height: '100px' }}
          value={query}
          onChange={(e) => setQuery(e.target.value)}
        />
      </div>

      <div className="text-center mb-4">
        <button
          onClick={handleRun}
          className="btn btn-primary btn-lg"
        >
          ▶️ Ejecutar
        </button>
      </div>

      <div>
        <label className="form-label fw-bold">📤 Resultado:</label>
        <pre className="form-control bg-dark text-success" style={{ height: '200px', overflow: 'auto' }}>
          {output || "(salida vacía)"}
        </pre>
      </div>
    </div>
  );
}
