// Many simultaneous requests for the small files every visit starts with (the large files are cached by each browser).
const URL = (process.env.SITE_URL || 'http://127.0.0.1:8008/').replace(/\/$/, '');
const N = +(process.env.REQUESTS || 1000);
(async () => {
  for (const path of ['/', '/app.json', '/shinylive-sw.js']) {
    const t = []; const codes = {};
    await Promise.all(Array.from({ length: N }, async () => { const s = Date.now();
      try { const r = await fetch(URL + path + '?r=' + Math.random(), { cache: 'no-store' }); await r.arrayBuffer(); codes[r.status] = (codes[r.status] || 0) + 1 } catch (e) { codes.error = (codes.error || 0) + 1 }
      t.push(Date.now() - s) }));
    t.sort((a, b) => a - b); const q = (x) => t[Math.min(t.length - 1, Math.floor(x * t.length))];
    console.log('BURST', path, N, 'simultaneous requests:', JSON.stringify(codes), 'ms median', q(.5), 'p95', q(.95), 'max', t[t.length - 1]);
    if ((codes[200] || 0) < N) process.exitCode = 1;
  }
})();
