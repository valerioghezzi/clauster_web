// Several people at once: each browser context opens the site, loads the example data and runs an analysis.
const { chromium } = require('playwright');
const URL = process.env.SITE_URL || 'http://127.0.0.1:8008/', USERS = +(process.env.USERS || 4), TAG = process.env.TAG || 'm';
(async () => {
  const b = await chromium.launch();
  const one = async (u) => {
    const ctx = await b.newContext({ viewport: { width: 1280, height: 860 } }); const p = await ctx.newPage(); const t0 = Date.now(); let f = null;
    await p.goto(URL);
    for (let i = 0; i < 450 && !f; i++) { await p.waitForTimeout(2000); for (const fr of p.frames()) { try { if (await fr.$('#example')) { f = fr; break } } catch (e) {} } }
    if (!f) return { user: `${TAG}-${u}`, ok: false, step: 'load', seconds: Math.round((Date.now() - t0) / 1000) };
    const ready = Math.round((Date.now() - t0) / 1000);
    await f.click('#example'); await p.waitForTimeout(3000); await f.click('a[data-value="Analysis"]'); await p.waitForTimeout(800);
    const t1 = Date.now(); await f.click('#run'); let s = '';
    for (let i = 0; i < 300; i++) { await p.waitForTimeout(2000); s = await f.textContent('#status'); if (/Completed|could not|stopped|rror/i.test(s)) break }
    const ok = /Completed/.test(s); await ctx.close();
    return { user: `${TAG}-${u}`, ok, ready_s: ready, analysis_s: Math.round((Date.now() - t1) / 1000), status: s.trim().slice(0, 80) };
  };
  const res = await Promise.all(Array.from({ length: USERS }, (_, u) => one(u + 1).catch(e => ({ user: `${TAG}-${u + 1}`, ok: false, error: e.message }))));
  for (const r of res) console.log('USER', JSON.stringify(r));
  await b.close(); process.exit(res.every(r => r.ok) ? 0 : 1);
})();
