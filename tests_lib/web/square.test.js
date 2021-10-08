require('expect-puppeteer');

describe('Futhark entry point : square test', () => {
  beforeAll(async () => {
    await page.goto('http://localhost:8000/square.html')
  })

  it('should display "1369" text on page', async () => {
    await expect(page).toMatch('1369')
  })
})
