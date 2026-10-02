// Loads the app in a browser (BROWSER: chromium, firefox or webkit; DEVICE: optional Playwright device name), runs two analyses and checks the downloads.
const pw = require('playwright');const engine = pw[process.env.BROWSER||'chromium'];const device = process.env.DEVICE ? pw.devices[process.env.DEVICE] : {viewport:{width:1366,height:900}};const fs=require('fs');fs.mkdirSync('shots',{recursive:true});
(async()=>{const b=await engine.launch();const p=await (await b.newContext(device)).newPage();console.log('browser',process.env.BROWSER||'chromium',process.env.DEVICE||'desktop 1366x900');
const log=[];p.on('console',m=>log.push(`[${m.type()}] ${m.text()}`.slice(0,300)));p.on('pageerror',e=>log.push('[pageerror] '+e.message));
const t0=Date.now();await p.goto(process.env.SITE_URL||'http://127.0.0.1:8008/');
const app=()=>{for(const f of p.frames()){if(f!==p.mainFrame()&&f.url().includes('app'))return f}return p.mainFrame()};
let ready=false;
for(let i=0;i<120&&!ready;i++){await p.waitForTimeout(5000);if(log.some(l=>/preload error:Error|object 'app_|CLAUSTER LOAD ERROR|could not load dynamic lib/.test(l)))break;for(const f of p.frames()){try{if(await f.$('#example')){ready=true;break}}catch(e){}}
  if(i%12==0){await p.screenshot({path:`shots/load-${i}.png`});console.log(`${Math.round((Date.now()-t0)/1000)} s: frames ${p.frames().length}`)}}
const dump=async(tag)=>{await p.screenshot({path:`shots/${tag}.png`,fullPage:true});for(const f of p.frames()){try{console.log('--- frame',f.url().slice(0,80),'\n',(await f.evaluate(()=>document.body?document.body.innerText:'')).slice(0,1500))}catch(e){}}
  console.log('--- console (errors and CLAuster messages)\n'+log.filter(l=>/CLAUSTER|rror|Warning|sys.call|source/.test(l)&&!/Downloading webR package/.test(l)).slice(-80).join('\n'))};
if(!ready){console.log('APP NOT READY after',Math.round((Date.now()-t0)/1000),'s');await dump('not-ready');await b.close();process.exit(1)}
console.log('app ready after',Math.round((Date.now()-t0)/1000),'s');
let f=null;for(const fr of p.frames()){if(await fr.$('#example')){f=fr;break}}
const fail=async(msg)=>{console.log('FAILED:',msg);await dump('failed');await b.close();process.exit(1)};
const run=async(label)=>{await f.click('a[data-value="Analysis"]');await p.waitForTimeout(1000);const t1=Date.now();await f.click('#run');let status='';
  for(let i=0;i<450;i++){await p.waitForTimeout(2000);status=await f.textContent('#status');if(/Completed|could not|stopped|rror/i.test(status))break}
  console.log(label,'status:',status.trim(),'| seconds:',Math.round((Date.now()-t1)/1000));if(!/Completed/.test(status))await fail(label+' did not complete');
  await f.click('a[data-value="Results"]');await p.waitForTimeout(6000);
  const findings=(await f.textContent('.findings').catch(()=>'')).replace(/\s+/g,' ');console.log(label,'findings:',findings.slice(0,160));
  if(!/Suggested solution/.test(findings))await fail(label+' shows no key findings');await p.screenshot({path:`shots/results-${label}.png`,fullPage:true})};
await f.click('#example');await p.waitForTimeout(4000);
const methods=await f.evaluate(()=>{const e=document.getElementById('method');return e&&e.selectize?Object.keys(e.selectize.options):Array.from(e.options).map(o=>o.value)});
console.log('methods:',methods.join(','));
const lack=['ward.D2','kmeans','pam','hybrid_kmeans','fuzzy','diana'].filter(m=>!methods.includes(m));if(lack.length)await fail('missing methods '+lack.join(','));
await run('ward');
const sig={report:'%PDF',data_export:'PK',export:'PK',plot_export:'%PDF'};
for(const id of Object.keys(sig)){
  const r=await f.evaluate(async(id)=>{const a=document.getElementById(id);const h=a&&a.getAttribute('href');if(!h)return {err:'no link'};
    try{const res=await fetch(h);const buf=new Uint8Array(await res.arrayBuffer());return {status:res.status,size:buf.length,head:String.fromCharCode(...buf.slice(0,4)),text:res.status==200?'':new TextDecoder().decode(buf.slice(0,300))}}catch(e){return {err:e.message}}},id);
  console.log('download',id,JSON.stringify(r));
  if(r.err||r.status!==200||r.size<200||!r.head.startsWith(sig[id]))await fail('download '+id)}
await f.evaluate(()=>document.getElementById('method').selectize.setValue('kmeans'));await p.waitForTimeout(1500);
await run('kmeans');
console.log('SMOKE TEST PASSED');await b.close()})();
