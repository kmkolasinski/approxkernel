from pathlib import Path
from typing import NamedTuple, Optional

import yaml


def obj_to_dict(obj: NamedTuple) -> dict:
    params = {}
    for field, value in obj._asdict().items():
        try:
            _ = value._fields  # this will raise Error for non tuple
            params[field] = obj_to_dict(value)
        except:
            params[field] = value
    return params


class GridConfig(NamedTuple):
    grid_size: int = 128
    num_scales: int = 4
    kernel_size: int = 17


class TrainConfig(NamedTuple):
    initial_lr: float = 0.005
    num_lr_steps: int = 4
    lr_decay: float = 0.5
    steps_per_lr: int = 1500


class Config(NamedTuple):
    kernel_fn_path: str
    debug_dir: Optional[str]
    clear_debug_dir: bool = False
    grid_config: GridConfig = GridConfig()
    train_config: TrainConfig = TrainConfig()

    @staticmethod
    def get_dummy_config():
        return Config(
            kernel_fn_path="kernel.py",
            debug_dir="debug"
        )

    def to_yaml(self, save_path: Path):
        params = obj_to_dict(self)

        with open(save_path, 'w') as fp:
            yaml.dump(params, fp, default_flow_style=False)

    @staticmethod
    def from_yaml(path: Path) -> "Config":
        with open(path, 'r') as fp:
            params = yaml.load(fp)

        return Config(
            kernel_fn_path=params["kernel_fn_path"],
            debug_dir=params.get("debug_dir"),
            clear_debug_dir=params["clear_debug_dir"],
            grid_config=GridConfig(**params["grid_config"]),
            train_config=TrainConfig(**params["train_config"])
        )
