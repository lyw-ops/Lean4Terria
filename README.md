# Lean4Terria

一个受《泰拉瑞亚》启发的单人横版沙盒。用 Lean 4 写地形、碰撞、挖掘、合成和存档规则，通过 raylib 连接窗口、键鼠和图形输出。

v0.1 从一把基础镐开始。向右下洞，站在第一排橙色铜矿旁，用左键挖三块。回程跳上台阶，走到工作台附近按 E 做铜镐。再沿洞穴向右，挖到浅绿色水晶。挖出的通道、搭起的台阶和背包里的物品可以保存，下次接着玩。

## 构建和运行

桌面版需要 Apple Silicon Mac、Xcode Command Line Tools 和 [elan](https://lean-lang.org/install/)。仓库锁定 Lean 4.35.0。构建脚本下载并校验 raylib 6.2 的官方 macOS 静态库。

```sh
./scripts/build-macos.sh
./.lake/build/bin/lean4terria
```

| 操作 | 按键 |
| --- | --- |
| 移动与跳跃 | A/D 或左右方向键，空格跳跃 |
| 挖掘和放置 | 左键用镐挖掘，右键放置选中的泥土或石头 |
| 物品与合成 | 1–6 选快捷栏，在工作台附近按 E 合成 |
| 存档与读档 | S 存档，L 读档，Esc 退出 |

再次打开游戏时会自动读取已有存档。默认存档是 `~/Library/Application Support/Lean4Terria/world.json`。用 `--seed N` 可以选择另一个种子并使用独立存档。用 `--save FILE` 可以指定存档位置。

## 测试和打包

```sh
lake exe lean4terria_test
./.lake/build/bin/lean4terria --smoke
./scripts/package-macos.sh
```

第一条命令运行纯 Lean 测试，覆盖挖铜、合成、采水晶和存档往返。第二条命令打开窗口运行桌面测试并打印帧时间。打包脚本生成 `dist/Lean4Terria-macos-arm64.zip`，内含经 ad-hoc 签名的 `.app`，供本地试玩。建议在本地非 iCloud 同步目录解压，以免 Finder 元数据影响签名校验。

技术设计和后续开发见[开发路线图](docs/roadmap.md)。
