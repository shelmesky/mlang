mlang 是一个静态类型的编程语言，支持编译到二进制可执行文件，支持多种CPU平台和可执行文件格式。



目前支持以下平台：

- Linux amd64 ELF 可执行文件
- Windows amd64 PE 可执行文件



在 Linux 平台下，支持编译到静态可执行文件。



![](https://raw.githubusercontent.com/shelmesky/mlang/master/files/compile_slice_index.png)



支持以下语法特性：



**变量声明**

```rust
namespace main

use "format"

function main() -> int64 {
    let a: int = 1
    let b,c: int = 2
    let d,e,f: int = 3,4,5
    return 0
}
```





**函数**

```rust
namespace main

use "format"

function add_two(aaa: uint64, bbb: uint64) -> uint64 {
    let a,b,c: uint64 = 123
    return aaa + 123
}

function print_string(str: string) {
    printf("%s\n", str)
}

function main() -> uint64 {
    let x: string = "%s\n"
    let a: string = "你好, mlang"
    printf(x, a)
    print_string(a)
    printf("%d\n", add_two(123, 456))
    let b: string
    return 0
}
```



**指针**



```rust
namespace main

use "format"

function main() -> uint64 {
    let ptr: *int
    let a: int = 123
    ptr = &a

    let ptr1: **int
    ptr1 = &ptr

    let ptr2: ***int
    ptr2 = &ptr1

    let b: int
    b = ***ptr2

    printf("%d\n", b)
    printf("%d\n", *ptr)
    printf("%d\n", **ptr1)
    printf("%d\n", ***ptr2)

    return 0
}
```





**数组**

```rust
namespace main

use "format"

function func() -> int {
    return 456
}

function main() -> uint64 {
    let b: int = 123;
    let a: [4]int = [4]int{1, 2, func(), b}

    let c: string = "ccc"
    let d: [3]string = [3]string{"aaa", "bbb", c}

    let e: [2][2]int = [2][2]int{{1,2}, {3,4}}

    let f: [2][2][2]int = [2][2][2]int{{{1,2},{3,4}}, {{5,6},{7,8}}}

    printf("%d\n", e[0][0])
    e[0] = [2]int{111, 222}
    printf("%d\n", e[0][0])

    return 0
}
```



**多维数组的声明和访问**

```rust
namespace main

use "format"

function main() -> int {
    // array index
    let a: [3]int = [3]int{1,2,3}
    let b: int = a[0]
    let c: int = a[1]
    let d: int = a[2]
    printf("[%d, %d, %d]\n", b, c, d)

    let a1: [3][3]int = [3][3]int{{111,222,333}, {444,555,666}, {777,888,999}}
    let a2: [3]int = a1[0]
    let a3: int = a2[1]
    printf("%d\n", a3)

    let a4: [3][3][3]int = [3][3][3]int{{{101,102,103},{104,105,106},{107,108,109}},{{110,111,112},{113,114,115},
    {116,117,118}}, {{119,120,121},{122,123,124},{125,126,127}}}
    let a5: [3][3]int = a4[0]
    let a6: [3]int = a5[0]
    let a7: int = a6[0]
    printf("%d\n", a7)

    let a8: int = a4[0][0][1];
    printf("%d\n", a8)

    return 0;
}
```



**列表**

```rust
namespace main

use "format"

function func() -> int {
    return 456
}

function main() -> int {
    let a: int = 123
    let b: []int = []int{}
    let c: []int = []int{1, 2, 3, 4, a, func()}

    let d: [][]int = [][]int{{1,2,3}, {4,5,6}}
    let e: [][][]int = [][][]int{{{111, 222}}}
    let f: [][][]int = [][][]int{{{111,333,52,32,2,2,2,4,3},
        {123,333,3,4,3,3,3,3,3,3},
        {3,3,3,3,3,2,2,2,2,2},
        {3,3,3,3,3,3,3,3},
        {}, {}, {}, {}, {}, {}, {}}}
    return 0
}
```



**多维列表的声明和访问**

```rust
namespace main

use "format"

function main() -> int64 {
    let a: []int = []int{1,2,3}
    let b: int = a[1]
    printf("%d\n", b)

    let c: [][]int = [][]int{{1,2,3}, {4,5,6}}
    let d: []int = c[1]
    let e: int = d[2]
    printf("%d\n", e)

    let f: [][]int = [][]int{{1,2,3}, {4,5,6}}
    // this line will cause 'runtime panic:index out of range'
    let g: []int = f[6]

    return 0
}
```



**for循环**



```rust
namespace main

use "format"

function main() -> int {
    let j: int = 0
    let k: int = 0

    for j=0; j<3; j=j+1 {
        printf("outer loop: %d\n", j)

        for k=0; k<3; k=k+1 {
            printf("inner loop: %d\n", k)
        }
    }

    let i: int = 0

    for i=0; i<10; i=i+1 {
        if i > 6 {
            break
        }
        printf("i value: %d\n", i)
    }

    let m: int = 0
    for m=0; m<10; m=m+1 {
        if m == 4 {
            continue
        }
        printf("m value: %d\n", m)
    }

    return 0
}
```



**if语句**

```rust
namespace main

use "format"

function main() -> int {
    let a: int = 1
    let b: int = 2
    let c: int = 3

    if a < b {
        if c > b {
            printf("%d < %d\n", a, b)
        }
    } else {
       printf("%d > %d\n", a, b)
    }

    return 0
}
```



正在开发的语言特性：

- 结构体类型
- map类型
- 指针运算
- 类型别名
- switch语句
- interface类型
- WebAssembly平台
