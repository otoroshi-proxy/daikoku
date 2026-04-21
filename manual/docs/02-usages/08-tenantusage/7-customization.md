# Tenant customization

Daikoku lets you customize several parts of the portal UI using CMS pages managed via the CLI. This allows you to tailor the look and content of your tenant without touching the source code.

For all CMS-based customizations, you create and push pages using the [Daikoku CLI](https://maif.github.io/daikoku/docs/cli).


## Footer

The portal footer can be customized per language using CMS pages at:

- `/customization/footer/fr` — displayed for French users
- `/customization/footer/en` — displayed for English users

**CLI project structure:**

```
src/pages/customization/footer/fr/page.html
src/pages/customization/footer/en/page.html
```

You can use Handlebars directives inside the footer as well:

```html
<footer>
  <p>© {{tenant.name}} — <a href="/contact">Contact</a></p>
</footer>
```

Push your pages with the CLI:

```bash
daikoku push
```
