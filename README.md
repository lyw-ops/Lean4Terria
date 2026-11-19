# Lean4Terria

用 Lean 4 手工复刻《泰拉瑞亚》桌面版 1.4.5.8 普通难度（Classic）。原版 C# 逻辑逐段转译成 Lean，保留随机数调用、分支和更新顺序，并用同版原版逐帧对照。窗口、键鼠和图形输出通过 raylib 接入。贴图、界面和字体直接读取本机 Steam 安装的原版 XNB 素材，仓库不附带任何原版素材。

## 当前状态

项目有两条游戏路线：

- **原版世界路线**（`--original-world`）：读取原版的 `.wld`/`.plr` 存档，用转译的原版逻辑运行，这是项目的主线。已接入地形挖掘与放置、液体、储物与合成、森林的昼夜和天气、部分敌怪与地牢、新手武器、死亡与墓碑等，F5 导出的副本能被原版读回。光照、物品动作、世界生成等模块正在逐个转译并接线。见 [原版世界预览](docs/original-world.md)。
- **紧凑地图路线**（无参数启动）：早期自建的小地图冒险，从开局一直到石巨人，但规则只是近似原版。见 [紧凑地图版攻略](docs/campaign-guide.md)。

完整的原版游戏还远没有完成。进度和分工见 [开发路线图](docs/roadmap.md) 和 [交接记录](HANDOFF.md)。

## 构建和运行

需要 Apple Silicon Mac、Xcode Command Line Tools、[elan](https://lean-lang.org/install/)，以及本机安装的 Terraria。仓库锁定 Lean 4.35.0。构建脚本会下载 raylib 6.2 的官方 macOS 静态库并校验。

```sh
./scripts/build-macos.sh
./.lake/build/bin/lean4terria                                  # 紧凑地图路线
./.lake/build/bin/lean4terria --original-world FILE.wld \
  --player FILE.plr --export-world out.wld --export-player out.plr   # 原版世界路线
```

如果原版装在另一个 Steam 库，启动前把 `LEAN4TERRIA_ORIGINAL_CONTENT` 设为 `Terraria.app/Contents/Resources/Content` 的完整路径。缺少原版基础素材时程序会提示并停止启动。

## 测试和打包

```sh
lake exe lean4terria_test
./.lake/build/bin/lean4terria --smoke
./scripts/package-macos.sh
```

试玩包只含应用和许可文件。各模块与原版的逐帧对照证据在本地 `.lake/` 下，不纳入版本库。

## 参考

机制和物品以 [Terraria 官方 Wiki](https://terraria.wiki.gg/) 为查询起点。Wiki 与程序实际表现不一致时，以 1.4.5.8 原版的实现和实测为准。
