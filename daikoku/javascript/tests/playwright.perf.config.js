import { defineConfig } from '@playwright/test';

export default defineConfig({
  timeout: 180_000,
  expect: {
    timeout: 5000
  },
  testDir: './specs-perf',
  fullyParallel: false,
  forbidOnly: !!process.env.CI,
  retries: 0,
  workers: 1,
  reporter: 'html',
  use: {
    actionTimeout: 180_000,
    trace: 'on-first-retry',
  },
});
