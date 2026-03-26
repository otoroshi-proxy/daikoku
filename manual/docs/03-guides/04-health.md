# Health Check Endpoints

Daikoku exposes two health check endpoints to monitor the platform status.

## `GET /health`

Basic health check endpoint. It verifies connectivity to the database (PostgreSQL).

### Access

**No authentication required** — this endpoint is public.

It is also compatible with the Otoroshi health check mechanism: if the `Otoroshi-Health-Check-Logic-Test` header is present, the response includes an `Otoroshi-Health-Check-Logic-Test-Result` header with the expected value.

### Response

```json
{
  "status": "ready"
}
```

If the database is not reachable:

```json
{
  "status": "initializing"
}
```

---

## `GET /health/details`

Detailed health check endpoint. It checks the status of each component for every tenant: database, mailer, S3 storage, and Otoroshi connections.

### Access

**Protected by access key** — the `access_key` query parameter is required.

```
GET /health/details?access_key=<your_key>
```

The key is configured in the application configuration:

```
daikoku.health.accessKey = "your_secret_key"
```

See the [configuration file reference](../01-getstarted/05-firstrun/configfile.md#common-configuration) for details on how to set up this property.

If the key is missing or invalid, the endpoint returns a `401 Unauthorized` error.

### Response

```json
{
  "status": "UP",
  "datastore": "UP",
  "version": "x.y.z",
  "MyTenant": {
    "tenantMode": "Default",
    "status": {
      "mailer": "UP",
      "S3": "UP",
      "otoroshi": [
        { "https://otoroshi.example.com (otoroshi-api.example.com)": "UP" }
      ]
    }
  }
}
```

### Possible statuses

| Status   | Meaning                                              |
|----------|------------------------------------------------------|
| `UP`     | The service is operational                           |
| `DOWN`   | The service is unreachable or in error               |
| `ABSENT` | The service is not configured for this tenant (e.g. no S3 bucket) |

### Services checked per tenant

- **mailer** — tests the connection to the email service
- **S3** — verifies access to the storage bucket (if configured)
- **otoroshi** — verifies the connection to each configured Otoroshi instance
