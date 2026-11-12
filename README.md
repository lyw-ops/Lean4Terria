# Lean4Terria

一个受《泰拉瑞亚》启发的单人横版沙盒。用 Lean 4 写地形、碰撞、挖掘、合成和存档规则，通过 raylib 连接窗口、键鼠和图形输出。

## v0.2 试玩

1. 在工作台附近按 **F**，用一份木材做三支火把。选中火把可以照亮身边，右键点击紧邻实心方块的空格可以放置火把。挖掉最后一个支撑块时会同时回收火把。
2. 向右下洞，挖三块橙色铜矿和两块石头。跳上台阶回工作台，按 **E** 做铜镐，按 **R** 用两块石头和一份木材做石剑。
3. 用铜镐挖开浅绿色水晶矿，继续往右找洞穴爬虫。选中石剑，朝爬虫的方向点击左键。生命耗尽时会在安全位置重生，并保留物品和地形改动。

## 构建和运行

桌面版需要 Apple Silicon Mac、Xcode Command Line Tools 和 [elan](https://lean-lang.org/install/)。仓库锁定 Lean 4.35.0。构建脚本下载并校验 raylib 6.2 的官方 macOS 静态库。

```sh
./scripts/build-macos.sh
./.lake/build/bin/lean4terria
```

| 操作 | 按键 |
| --- | --- |
| 移动与跳跃 | A/D 或左右方向键，空格跳跃 |
| 挖掘、建造和战斗 | 左键使用镐或剑，右键放置选中的方块或火把 |
| 物品与合成 | 1–8 选快捷栏。工作台附近按 E 做铜镐、F 做火把、R 做石剑 |
| 存档与读档 | S 存档，L 读档，Esc 退出 |

再次打开游戏时会自动读取已有存档。v0.1 的地形、位置和物品会保留，快捷栏会扩到八格并补充四份木材。下一次按 S 存档时才会写入新版格式。默认存档是 `~/Library/Application Support/Lean4Terria/world.json`。用 `--seed N` 可以选择另一个种子并使用独立存档。用 `--save FILE` 可以指定存档位置。

## 测试和打包

```sh
lake exe lean4terria_test
./.lake/build/bin/lean4terria --smoke
./scripts/package-macos.sh
```

第一条命令运行纯 Lean 测试，覆盖挖矿、合成、光照遮挡、战斗、重生和存档迁移。第二条命令打开窗口运行桌面测试并打印帧时间。打包脚本生成 `dist/Lean4Terria-v0.2.0-macos-arm64.zip`，内含经 ad-hoc 签名的 `.app`，供本地试玩。建议在本地非 iCloud 同步目录解压，以免 Finder 元数据影响签名校验。

技术设计和后续开发见[开发路线图](docs/roadmap.md)。
