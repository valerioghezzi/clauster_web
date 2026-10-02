# Opens the site in the real Firefox release (Selenium + geckodriver), runs the example analysis and checks the result.
import os, sys, time
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.firefox.options import Options
url = os.environ.get("SITE_URL", "http://127.0.0.1:8008/")
o = Options(); o.add_argument("-headless")
d = webdriver.Firefox(options=o); d.set_window_size(1366, 900); d.set_page_load_timeout(120)
t0 = time.time(); d.get(url); print("browser:", d.capabilities.get("browserName"), d.capabilities.get("browserVersion"))
def find_app():
    d.switch_to.default_content()
    for f in d.find_elements(By.TAG_NAME, "iframe"):
        d.switch_to.default_content(); d.switch_to.frame(f)
        if d.find_elements(By.ID, "example"): return True
        for g in d.find_elements(By.TAG_NAME, "iframe"):
            d.switch_to.frame(g)
            if d.find_elements(By.ID, "example"): return True
            d.switch_to.parent_frame()
    d.switch_to.default_content(); return False
ok = False
for i in range(120):
    time.sleep(5)
    try: ok = find_app()
    except Exception as e: ok = False
    if ok: break
print("app ready:", ok, "after", round(time.time() - t0), "s")
if not ok:
    d.switch_to.default_content(); print(d.page_source[:1500]); d.save_screenshot("shots/firefox-real.png"); d.quit(); sys.exit(1)
d.find_element(By.ID, "example").click(); time.sleep(4)
d.find_element(By.CSS_SELECTOR, 'a[data-value="Analysis"]').click(); time.sleep(1)
t1 = time.time(); d.find_element(By.ID, "run").click(); status = ""
for i in range(300):
    time.sleep(2); status = d.find_element(By.ID, "status").text
    if any(w in status for w in ("Completed", "could not", "stopped", "rror")): break
print("status:", status, "| seconds:", round(time.time() - t1))
d.find_element(By.CSS_SELECTOR, 'a[data-value="Results"]').click(); time.sleep(6)
findings = " ".join(d.find_element(By.CSS_SELECTOR, ".findings").text.split())
print("findings:", findings[:160]); d.save_screenshot("shots/firefox-real-results.png"); d.quit()
if "Completed" not in status or "Suggested solution" not in findings: sys.exit(1)
print("REAL FIREFOX TEST PASSED")
