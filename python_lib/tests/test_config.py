from pathlib import Path
from unittest import TestCase
import tempfile

import approxkernel.config as cfg


class TestConfig(TestCase):
    def setUp(self):
        self._tmpdir = Path(tempfile.mkdtemp())

    def test_save_load(self):
        config = cfg.Config("path_fn", "debug")
        config.to_yaml(self._tmpdir / "config.yaml")
        new_config = cfg.Config.from_yaml(self._tmpdir / "config.yaml")
        self.assertEqual(config, new_config)
