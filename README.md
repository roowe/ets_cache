erlang游戏服务器使用的cache模块
=====

初衷
--------
查看其他玩家数据（热点玩家的数据想活跃在内存，能加速访问，冷的玩家被访问之后，过了TTL之后，会自动从内存清除）或者binary编译出来的关键字MP要找个共享地方存，从而有这样的一个模块，本身就一个项目在用，后来多个项目一起用，复制粘帖太麻烦了，就独立出来，欢迎fork。

update Tue Aug 19 15:30:50 2014

本来，这个模块只打算存小规模（几万条，甚至峰值也就十万多规模）的数据，所以最初设计就是一张ETS搞定，但是随着项目比较重度使用这个模块（预计长度会超百万），怕一张ETS表清理比较耗时，而且不同业务对清理的时间间隔要求也不一样，有些频率会稍微高点。所以重构代码，支持多表，开多进程去维护各自的TTL。接口向下兼容，新增了set_t、new_cache_table等函数，废弃老的get/2的接口，老的设计这个接口是自动去mnesia取数据，业务层可以使用get_with_default这个更通用的接口替换get/2，但是又新增了get/2，这个是传自己的cache表名和key。

功能
--------
1. time to live 定时清理，ets_cache_manager负责，默认是5分钟，可以通过application的env配置
2. 并发读/写，从而会有资源竞争

定位
--------
1. 这个项目主要可以用来存一些全局数据，又懒得开ets去维护，那就交给ets_cache就可以了，它本身就做好这个。
2. 临时不怎么重要的数据，一致性，准确性不怎么高，为了加速访问，同时需要自动清理。

其他时间，你或许更应该使用ets，而不是我这个项目。
安装
--------

在你的`rebar.config`添加:

    {ets_cache, ".*", {git, "https://github.com/roowe/ets_cache", "master"}}

之后执行 `rebar get-deps`接着 `rebar compile`.

用法
--------
```erlang
1> application:start(ets_cache).
ok
2> ets_cache:new_cache_table(test_cache_table, 2).
{ok,<0.47.0>}
3> ets_cache:get(test_key).
[]
4> ets_cache:set(test_key, 1).
ok
5> ets_cache:get(test_key).
1
6> ets_cache:set_t(test_cache_table, test_key, 1).
ok
7> ets_cache:get(test_cache_table, test_key).
1
```

其他可以看test/ets_cache_tests.erl

单元测试
--------
`rebar eunit`
