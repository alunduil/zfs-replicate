# Configuration

The project uses a JSON schema for settings and a YAML config
for deployment options. Each module validates its own section.

```python
def configure_engine_config_registry(settings):
    """Configure the engine with the provided settings."""
    return settings.apply_to_engine()
```

## Verify

Run `poetry run pytest` to verify all modules pass.
