"""Open the deployed app in a headless browser so it refreshes its data (run each morning by
.github/workflows/wake-nba-app.yml). A plain HTTP request isn't enough: Streamlit only runs the app for a
browser session, and a sleeping app first shows a wake-up button. Run: python wake.py"""
import sys

from playwright.sync_api import sync_playwright

URL = "https://matteomagcalas-fb26.streamlit.app/"
DONE = "ESPN data updated"  # sidebar caption, drawn once data() has finished (refresh included)


def main():
    with sync_playwright() as p:
        page = p.chromium.launch().new_page()
        page.goto(URL, timeout=120_000)
        for _ in range(60):  # up to ~10 minutes: wake-up plus a full ESPN refresh
            wake = page.get_by_role("button", name="Yes, get this app back up!")
            if wake.count():
                print("app was asleep, waking it")
                wake.click()
            if any(DONE in f.content() for f in page.frames if not f.is_detached()):
                print("app loaded, data is fresh")
                return 0
            page.wait_for_timeout(10_000)
        print("timed out waiting for the app")
        return 1


if __name__ == "__main__":
    sys.exit(main())
