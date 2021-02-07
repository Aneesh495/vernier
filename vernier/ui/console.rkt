#lang racket/base

;;; vernier/ui/console.rkt
;;; Embedded standalone Web Console Single Page Application (SPA).
;;; Provides real-time waterfall inspection, critical path views, and topology maps.

(require racket/contract)

(provide
 (contract-out
  [render-console-html (-> string?)]))

(define (render-console-html)
  #<<HTML
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Vernier: Distributed Tracing & Telemetry Console</title>
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <style>
    :root {
      --bg: #090d16;
      --surface: #111827;
      --surface-border: #1f2937;
      --surface-hover: #1e293b;
      --text: #f3f4f6;
      --text-muted: #9ca3af;
      --primary: #38bdf8;
      --primary-dim: rgba(56, 189, 248, 0.15);
      --critical: #f87171;
      --critical-dim: rgba(248, 113, 113, 0.15);
      --success: #34d399;
      --warning: #fbbf24;
      --font-mono: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
      --font-sans: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    }
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      background: var(--bg);
      color: var(--text);
      font-family: var(--font-sans);
      font-size: 14px;
      line-height: 1.5;
      display: flex;
      flex-direction: column;
      height: 100vh;
      overflow: hidden;
    }
    header {
      background: var(--surface);
      border-bottom: 1px solid var(--surface-border);
      padding: 12px 24px;
      display: flex;
      align-items: center;
      justify-content: space-between;
      flex-shrink: 0;
    }
    .brand {
      display: flex;
      align-items: center;
      gap: 12px;
      font-weight: 700;
      font-size: 18px;
      letter-spacing: -0.5px;
    }
    .brand-badge {
      background: var(--primary-dim);
      color: var(--primary);
      font-size: 11px;
      font-weight: 600;
      padding: 2px 8px;
      border-radius: 9999px;
      border: 1px solid rgba(56, 189, 248, 0.3);
      text-transform: uppercase;
    }
    .stats-bar {
      display: flex;
      align-items: center;
      gap: 20px;
      font-family: var(--font-mono);
      font-size: 12px;
    }
    .stat-pill {
      display: flex;
      align-items: center;
      gap: 6px;
      color: var(--text-muted);
    }
    .stat-val {
      color: var(--text);
      font-weight: 600;
    }
    .pulse-dot {
      width: 8px;
      height: 8px;
      border-radius: 50%;
      background: var(--success);
      box-shadow: 0 0 8px var(--success);
    }
    nav {
      background: var(--surface);
      border-bottom: 1px solid var(--surface-border);
      padding: 0 24px;
      display: flex;
      gap: 24px;
      flex-shrink: 0;
    }
    .nav-btn {
      background: none;
      border: none;
      color: var(--text-muted);
      padding: 12px 0;
      font-size: 13px;
      font-weight: 500;
      cursor: pointer;
      position: relative;
      transition: color 0.15s;
    }
    .nav-btn:hover { color: var(--text); }
    .nav-btn.active {
      color: var(--primary);
      font-weight: 600;
    }
    .nav-btn.active::after {
      content: "";
      position: absolute;
      bottom: -1px;
      left: 0;
      right: 0;
      height: 2px;
      background: var(--primary);
    }
    main {
      flex: 1;
      overflow-y: auto;
      padding: 24px;
      display: flex;
      flex-direction: column;
      gap: 20px;
    }
    .search-box {
      display: flex;
      gap: 12px;
      align-items: center;
    }
    .search-input {
      flex: 1;
      background: var(--surface);
      border: 1px solid var(--surface-border);
      color: var(--text);
      padding: 10px 16px;
      border-radius: 8px;
      font-family: var(--font-mono);
      font-size: 13px;
      outline: none;
      transition: border-color 0.15s;
    }
    .search-input:focus { border-color: var(--primary); }
    .btn {
      background: var(--primary);
      color: #000;
      font-weight: 600;
      padding: 10px 18px;
      border-radius: 8px;
      border: none;
      cursor: pointer;
      font-size: 13px;
      transition: opacity 0.15s;
    }
    .btn:hover { opacity: 0.9; }
    .filter-pills {
      display: flex;
      gap: 8px;
      flex-wrap: wrap;
    }
    .pill {
      background: var(--surface);
      border: 1px solid var(--surface-border);
      color: var(--text-muted);
      padding: 4px 10px;
      border-radius: 6px;
      font-size: 12px;
      cursor: pointer;
      font-family: var(--font-mono);
    }
    .pill:hover { border-color: var(--primary); color: var(--text); }
    .card {
      background: var(--surface);
      border: 1px solid var(--surface-border);
      border-radius: 10px;
      overflow: hidden;
    }
    table {
      width: 100%;
      border-collapse: collapse;
      text-align: left;
      font-size: 13px;
    }
    th {
      background: rgba(255, 255, 255, 0.02);
      color: var(--text-muted);
      font-weight: 600;
      padding: 12px 16px;
      border-bottom: 1px solid var(--surface-border);
      text-transform: uppercase;
      font-size: 11px;
      letter-spacing: 0.5px;
    }
    td {
      padding: 12px 16px;
      border-bottom: 1px solid var(--surface-border);
      font-family: var(--font-mono);
      font-size: 12px;
    }
    tr:hover td { background: var(--surface-hover); cursor: pointer; }
    .badge {
      padding: 2px 6px;
      border-radius: 4px;
      font-size: 11px;
      font-weight: 600;
    }
    .badge-ok { background: rgba(52, 211, 153, 0.15); color: var(--success); }
    .badge-error { background: rgba(248, 113, 113, 0.15); color: var(--critical); }
    .waterfall-container {
      display: flex;
      flex-direction: column;
      gap: 8px;
      padding: 16px;
    }
    .waterfall-row {
      display: grid;
      grid-template-columns: 280px 1fr 100px;
      align-items: center;
      gap: 16px;
      padding: 6px 10px;
      border-radius: 6px;
      font-family: var(--font-mono);
      font-size: 12px;
      background: rgba(255, 255, 255, 0.01);
    }
    .waterfall-row:hover { background: var(--surface-hover); }
    .waterfall-track {
      position: relative;
      height: 18px;
      background: rgba(255, 255, 255, 0.03);
      border-radius: 3px;
      overflow: hidden;
    }
    .waterfall-bar {
      position: absolute;
      top: 0;
      bottom: 0;
      border-radius: 3px;
      background: var(--primary);
    }
    .waterfall-bar.critical {
      background: var(--critical);
      box-shadow: 0 0 8px rgba(248, 113, 113, 0.4);
    }
    .waterfall-bar.error { background: #e11d48; }
    .tag-grid {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(240px, 1fr));
      gap: 12px;
      padding: 16px;
    }
    .tag-card {
      background: rgba(255, 255, 255, 0.02);
      border: 1px solid var(--surface-border);
      padding: 8px 12px;
      border-radius: 6px;
      font-family: var(--font-mono);
      font-size: 12px;
    }
    .tab-content { display: none; }
    .tab-content.active { display: flex; flex-direction: column; gap: 20px; }
  </style>
</head>
<body>
  <header>
    <div class="brand">
      <span style="color:var(--primary)">❖</span>
      <span>VERNIER</span>
      <span class="brand-badge">Telemetry Engine</span>
    </div>
    <div class="stats-bar">
      <div class="stat-pill"><div class="pulse-dot"></div><span>LIVE</span></div>
      <div class="stat-pill">Spans: <span class="stat-val" id="stat-spans">0</span></div>
      <div class="stat-pill">Traces: <span class="stat-val" id="stat-traces">0</span></div>
      <div class="stat-pill">Services: <span class="stat-val" id="stat-services">0</span></div>
    </div>
  </header>

  <nav>
    <button class="nav-btn active" onclick="switchTab('explorer')">Trace Explorer</button>
    <button class="nav-btn" onclick="switchTab('inspector')">Trace Inspector</button>
    <button class="nav-btn" onclick="switchTab('topology')">Service Topology</button>
    <button class="nav-btn" onclick="switchTab('stream')">Live Stream</button>
  </nav>

  <main>
    <!-- Tab 1: Trace Explorer -->
    <div id="tab-explorer" class="tab-content active">
      <div class="search-box">
        <input type="text" id="vql-input" class="search-input" placeholder="Filter with VQL: e.g. service:gateway AND status:error AND duration > 100ms" value="">
        <button class="btn" onclick="searchTraces()">Execute Query</button>
      </div>
      <div class="filter-pills">
        <span class="pill" onclick="setQuery('status:error')">Errors Only</span>
        <span class="pill" onclick="setQuery('duration > 150ms')">Latency > 150ms</span>
        <span class="pill" onclick="setQuery('service:payments')">Payments Service</span>
        <span class="pill" onclick="setQuery('')">Clear Filters</span>
      </div>
      <div class="card">
        <table>
          <thead>
            <tr>
              <th>Trace ID</th>
              <th>Root Operation</th>
              <th>Service</th>
              <th>Duration</th>
              <th>Spans</th>
              <th>Status</th>
              <th>Started</th>
            </tr>
          </thead>
          <tbody id="trace-table-body">
            <tr><td colspan="7" style="text-align:center; color:var(--text-muted);">No traces loaded. Submit or generate traces to explore.</td></tr>
          </tbody>
        </table>
      </div>
    </div>

    <!-- Tab 2: Trace Inspector -->
    <div id="tab-inspector" class="tab-content">
      <div class="card" style="padding: 16px;">
        <div style="display:flex; justify-content:space-between; align-items:center;">
          <div>
            <h3 id="detail-title" style="font-size:16px; margin-bottom:4px;">No Trace Selected</h3>
            <div id="detail-subtitle" style="font-family:var(--font-mono); color:var(--text-muted); font-size:12px;">Select a trace from Explorer or Live Stream to inspect the execution graph.</div>
          </div>
          <div id="detail-badges" style="display:flex; gap:8px;"></div>
        </div>
      </div>
      <div class="card">
        <div style="padding: 12px 16px; border-bottom: 1px solid var(--surface-border); font-weight:600; font-size:12px; color:var(--text-muted); text-transform:uppercase;">
          Execution Timeline & Critical Path
        </div>
        <div id="waterfall-box" class="waterfall-container">
          <div style="color:var(--text-muted); text-align:center; padding:20px;">No active trace loaded.</div>
        </div>
      </div>
      <div class="card">
        <div style="padding: 12px 16px; border-bottom: 1px solid var(--surface-border); font-weight:600; font-size:12px; color:var(--text-muted); text-transform:uppercase;">
          Root Cause & Anomaly Attribution
        </div>
        <div id="anomaly-box" style="padding:16px; font-family:var(--font-mono); font-size:13px;">
          Select a trace to view anomaly scoring and latency bottleneck attribution.
        </div>
      </div>
    </div>

    <!-- Tab 3: Topology -->
    <div id="tab-topology" class="tab-content">
      <div class="card" style="padding:24px; min-height:400px; display:flex; flex-direction:column; gap:16px;">
        <div style="display:flex; justify-content:space-between; align-items:center;">
          <h3 style="font-size:15px;">Service Dependency Topology Map</h3>
          <button class="pill" onclick="loadTopology()">Refresh Topology</button>
        </div>
        <div id="topology-graph" style="flex:1; border: 1px solid var(--surface-border); border-radius:8px; min-height:350px; display:flex; align-items:center; justify-content:center; color:var(--text-muted); font-family:var(--font-mono);">
          Loading service graph...
        </div>
      </div>
    </div>

    <!-- Tab 4: Live Stream -->
    <div id="tab-stream" class="tab-content">
      <div class="card">
        <div style="padding: 12px 16px; border-bottom: 1px solid var(--surface-border); display:flex; justify-content:space-between; align-items:center;">
          <div style="font-weight:600; font-size:12px; color:var(--text-muted); text-transform:uppercase;">Server-Sent Events Feed</div>
          <span class="badge badge-ok">SSE ACTIVE</span>
        </div>
        <table>
          <thead>
            <tr>
              <th>Trace ID</th>
              <th>Root Operation</th>
              <th>Service</th>
              <th>Duration</th>
              <th>Spans</th>
              <th>Status</th>
            </tr>
          </thead>
          <tbody id="stream-table-body">
            <tr><td colspan="6" style="text-align:center; color:var(--text-muted);">Listening for real-time trace events...</td></tr>
          </tbody>
        </table>
      </div>
    </div>
  </main>

  <script>
    function switchTab(tabId) {
      document.querySelectorAll('.nav-btn').forEach(b => b.classList.remove('active'));
      document.querySelectorAll('.tab-content').forEach(c => c.classList.remove('active'));
      event.target.classList.add('active');
      document.getElementById('tab-' + tabId).classList.add('active');
      if (tabId === 'topology') loadTopology();
    }

    function setQuery(q) {
      document.getElementById('vql-input').value = q;
      searchTraces();
    }

    async function updateStats() {
      try {
        const res = await fetch('/api/v1/stats');
        const data = await res.json();
        document.getElementById('stat-spans').textContent = data.total_spans_indexed || 0;
        document.getElementById('stat-traces').textContent = data.total_traces || 0;
        document.getElementById('stat-services').textContent = data.distinct_services || 0;
      } catch (e) {}
    }

    async function searchTraces() {
      const q = document.getElementById('vql-input').value.trim();
      const url = q ? '/api/v1/traces?q=' + encodeURIComponent(q) : '/api/v1/traces';
      try {
        const res = await fetch(url);
        const data = await res.json();
        renderTraceTable(data.traces || []);
      } catch (e) {
        console.error(e);
      }
    }

    function renderTraceTable(traces) {
      const tbody = document.getElementById('trace-table-body');
      if (traces.length === 0) {
        tbody.innerHTML = '<tr><td colspan="7" style="text-align:center; color:var(--text-muted);">No matching traces found.</td></tr>';
        return;
      }
      tbody.innerHTML = traces.map(t => `
        <tr onclick="inspectTrace('${t.trace_id}')">
          <td style="color:var(--primary); font-weight:600;">${t.trace_id.substring(0, 16)}...</td>
          <td>${t.root_name || 'unknown'}</td>
          <td>${t.root_service || 'unknown'}</td>
          <td>${(t.duration / 1000000).toFixed(2)} ms</td>
          <td>${t.span_count}</td>
          <td><span class="badge ${t.error_count > 0 ? 'badge-error' : 'badge-ok'}">${t.error_count > 0 ? 'ERROR' : 'OK'}</span></td>
          <td>${new Date(t.start_time / 1000000).toLocaleTimeString()}</td>
        </tr>
      `).join('');
    }

    async function inspectTrace(tid) {
      try {
        const res = await fetch('/api/v1/traces/' + tid);
        const data = await res.json();
        document.getElementById('detail-title').textContent = (data.spans[0] ? data.spans[0].name : 'Trace') + ' (' + tid + ')';
        document.getElementById('detail-subtitle').textContent = `Total Duration: ${(data.duration_nanos / 1000000).toFixed(2)} ms | Spans: ${data.span_count} | Critical Duration: ${(data.critical_path.critical_duration_nanos / 1000000).toFixed(2)} ms`;
        
        document.getElementById('detail-badges').innerHTML = `
          <span class="badge ${data.error_count > 0 ? 'badge-error' : 'badge-ok'}">${data.error_count > 0 ? 'FAIL' : 'OK'}</span>
          <span class="badge ${data.anomaly_analysis.health === 'critical' ? 'badge-error' : 'badge-ok'}">HEALTH: ${data.anomaly_analysis.health.toUpperCase()}</span>
        `;

        // Render Waterfall
        const wBox = document.getElementById('waterfall-box');
        wBox.innerHTML = data.waterfall.map(w => `
          <div class="waterfall-row">
            <div style="padding-left: ${w.depth * 16}px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;">
              <span style="color:var(--primary); font-weight:600;">${w.service}</span>: ${w.operation}
            </div>
            <div class="waterfall-track">
              <div class="waterfall-bar ${w.critical ? 'critical' : ''} ${w.error ? 'error' : ''}" style="left:${w.offset_pct}%; width:${Math.max(1, w.width_pct)}%;"></div>
            </div>
            <div style="text-align:right; color: ${w.critical ? 'var(--critical)' : 'var(--text-muted)'}; font-weight:${w.critical ? '600' : '400'};">
              ${w.critical ? '[CRIT]' : ''}
            </div>
          </div>
        `).join('');

        // Anomaly box
        const aBox = document.getElementById('anomaly-box');
        aBox.innerHTML = `
          <div style="margin-bottom:8px;"><strong>Composite Anomaly Score:</strong> ${(data.anomaly_analysis.score * 100).toFixed(1)}% (${data.anomaly_analysis.primary_issue})</div>
          <div style="color:var(--text-muted); margin-bottom:12px;"><strong>Contributing Factors:</strong></div>
          <ul style="padding-left:20px; color:var(--text-muted);">
            ${data.anomaly_analysis.factors.map(f => `<li>${f}</li>`).join('') || '<li>No architectural or latency anomalies detected.</li>'}
          </ul>
        `;

        switchTab('inspector');
        document.querySelectorAll('.nav-btn')[1].classList.add('active');
        document.querySelectorAll('.nav-btn')[0].classList.remove('active');
      } catch (e) {
        console.error(e);
      }
    }

    async function loadTopology() {
      try {
        const res = await fetch('/api/v1/topology');
        const data = await res.json();
        const box = document.getElementById('topology-graph');
        if (!data.nodes || data.nodes.length === 0) {
          box.innerHTML = 'No service interactions recorded yet.';
          return;
        }
        box.innerHTML = `
          <div style="display:flex; flex-wrap:wrap; gap:16px; padding:20px; width:100%; justify-content:center;">
            ${data.nodes.map(n => `
              <div class="card" style="padding:12px 18px; min-width:160px; text-align:center;">
                <div style="font-weight:700; color:var(--primary); font-size:14px; margin-bottom:4px;">${n.service}</div>
                <div style="font-size:11px; color:var(--text-muted);">Calls: ${n.total_calls} | Errors: ${n.error_count}</div>
                <div style="margin-top:6px;"><span class="badge ${n.health === 'failing' ? 'badge-error' : 'badge-ok'}">${n.health.toUpperCase()}</span></div>
              </div>
            `).join('')}
          </div>
        `;
      } catch (e) {}
    }

    // Connect Server-Sent Events
    function initSSE() {
      const source = new EventSource('/api/v1/stream');
      source.addEventListener('trace', function(e) {
        const t = JSON.parse(e.data);
        const tbody = document.getElementById('stream-table-body');
        const trHtml = `
          <tr onclick="inspectTrace('${t.trace_id}')">
            <td style="color:var(--primary); font-weight:600;">${t.trace_id.substring(0, 16)}...</td>
            <td>${t.root_name}</td>
            <td>${t.root_service}</td>
            <td>${(t.duration_nanos / 1000000).toFixed(2)} ms</td>
            <td>${t.span_count}</td>
            <td><span class="badge ${t.error_count > 0 ? 'badge-error' : 'badge-ok'}">${t.error_count > 0 ? 'ERROR' : 'OK'}</span></td>
          </tr>
        `;
        if (tbody.innerHTML.includes('Listening for real-time')) {
          tbody.innerHTML = trHtml;
        } else {
          tbody.innerHTML = trHtml + tbody.innerHTML;
        }
        updateStats();
      });
    }

    window.onload = function() {
      searchTraces();
      updateStats();
      initSSE();
      setInterval(updateStats, 3000);
    };
  </script>
</body>
</html>
HTML
)
