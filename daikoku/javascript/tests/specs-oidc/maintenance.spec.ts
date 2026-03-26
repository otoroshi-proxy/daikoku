import test, { expect } from '@playwright/test';
import { MICHAEL } from './users';
import { loginOidcAs,  adminApikeyId, adminApikeySecret, exposedPort } from './utils';

test.beforeEach(async () => {
  await fetch(`http://localhost:${exposedPort}/admin-api/state/reset`, {
    method: 'POST',
    headers: {
      "Authorization": `Basic ${btoa(adminApikeyId + ":" + adminApikeySecret)}`
    }
  })
    .then(r => r.json())
});

const passInModeMaintenance  = async ({page}) => {
  await page.goto("http://localhost:5173/auth/Local/login")   
  await page.locator('input[name="username"]').fill('admin@foo.bar');
  await page.locator('input[name="password"]').fill('password');
  await page.getByRole('button', { name: 'Se connecter' }).click();
  await page.getByRole('button', { name: 'user menu' }).click();
  await page.getByRole('button', { name: 'user menu' }).click();
  await page.getByRole('button', { name: 'user menu' }).click();
  await page.getByRole('button', { name: 'user menu' }).click();
  await page.getByText('En maintenance').click();
  await page.getByRole('button', { name: 'user menu' }).click();
  await page.getByRole('link', { name: 'Déconnexion' }).click();
  expect(page.getByText('You are now logged out')).toBeVisible;
  await page.goto("http://localhost:5173/auth/Local/login")  
}

test('Passe en mode maintenance', async ({ page }) => {
  await passInModeMaintenance({page})
  expect (page.getByRole('heading', { name: 'Daikoku est en maintenance' })).toBeVisible;
});

test('Se connecter en maintenance avec admin', async ({ page }) => {
  await passInModeMaintenance({page})
  await loginOidcAs(MICHAEL,page)
  await page.goto("http://localhost:5173/apis") 
  await expect (page.getByRole('heading', { name: 'Mes APIs' })).toBeVisible;

});

test('redirection par l\'url retourne page maintenance', async ({ page }) => {
  await passInModeMaintenance({page})
  await page.goto("http://localhost:5173/auth/Local/login")
  expect (page.getByRole('heading', { name: 'Daikoku est en maintenance' })).toBeVisible;
});



