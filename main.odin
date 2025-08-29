package main

import "core:flags"
import "core:fmt"
import "core:math"
import "core:math/rand"
import "core:mem"
import "core:os"
import "core:path/filepath"
import "core:strconv"
import "core:strings"
import "core:sync"
import "core:thread"
import "vendor:stb/image"

Add :: struct {
  e1: ^Expr,
  e2: ^Expr,
}

Mul :: struct {
  e1: ^Expr,
  e2: ^Expr,
}

Pow :: struct {
  pow: int,
  e1:  ^Expr,
}

FunType :: enum {
  Sin,
  Abs,
  Sqrt,
  Log,
  Inv,
}

Fun :: struct {
  typ: FunType,
  e1:  ^Expr,
}

Cond :: enum {
  Equal,
  Less,
  GreaterEqual,
}

If :: struct {
  cond: Cond,
  a:    ^Expr,
  b:    ^Expr,
  c:    ^Expr,
  d:    ^Expr,
}

Expr :: union {
  f64,
  string,
  Add,
  Mul,
  Pow,
  Fun,
  If,
}

free_expr :: proc(e: ^Expr) {
  switch v in e {
  case f64:
    free(e)
  case string:
    free(e)
  case Add:
    free_expr(v.e1)
    free_expr(v.e2)
    free(e)
  case Mul:
    free_expr(v.e1)
    free_expr(v.e2)
    free(e)
  case Pow:
    free_expr(v.e1)
    free(e)
  case Fun:
    free_expr(v.e1)
    free(e)
  case If:
    free_expr(v.a)
    free_expr(v.b)
    free_expr(v.c)
    free_expr(v.d)
    free(e)
  }
}

print_expr :: proc(e: Expr) {
  print_expr_helper(e)
  fmt.println()
}

print_expr_helper :: proc(e: Expr) {
  switch v in e {
  case f64:
    fmt.printf("%v", v)
  case string:
    fmt.printf("%v", v)
  case Add:
    fmt.print("(")
    print_expr_helper(v.e1^)
    fmt.print(" + ")
    print_expr_helper(v.e2^)
    fmt.print(")")
  case Mul:
    fmt.print("(")
    print_expr_helper(v.e1^)
    fmt.print(" * ")
    print_expr_helper(v.e2^)
    fmt.print(")")
  case Pow:
    fmt.print("(")
    print_expr_helper(v.e1^)
    fmt.print(" ^ ")
    fmt.print(v.pow)
    fmt.print(")")
  case Fun:
    switch v.typ {
    case .Sin:
      fmt.print("sin(")
    case .Abs:
      fmt.print("abs(")
    case .Sqrt:
      fmt.print("sqrt(")
    case .Log:
      fmt.print("log(")
    case .Inv:
      fmt.print("inv(")
    }
    print_expr_helper(v.e1^)
    fmt.print(")")
  case If:
    fmt.print("if(")
    print_expr_helper(v.a^)
    switch v.cond {
    case .Equal:
      fmt.print(" == ")
    case .Less:
      fmt.print(" < ")
    case .GreaterEqual:
      fmt.print(" >= ")
    }
    print_expr_helper(v.b^)
    fmt.print(", ")
    print_expr_helper(v.c^)
    fmt.print(", ")
    print_expr_helper(v.d^)
    fmt.print(")")
  }
}

generate_function :: proc(params: []string, depth: int) -> (res: ^Expr, ok: bool) {
  variants := [7]int{1, 2, 3, 4, 5, 6, 7}
  short_variants := [2]int{1, 2}
  functions := [5]FunType{.Sin, .Abs, .Sqrt, .Log, .Inv}
  conditions := [3]Cond{.Equal, .Less, .GreaterEqual}
  powers := [2]int{2, 3}
  opts: ^Options = auto_cast context.user_ptr
  weights := convert_weights(opts.weights)
  n := rand.choice(short_variants[:])
  if depth > 0 {
    v := rand.float64_uniform(0, 1)
    for w, idx in weights {
      if v < w {
        n = idx + 1
        break
      }
    }
  }
  res = new(Expr)
  switch n {
  case 1:
    res^ = rand.float64_uniform(-opts.constant, opts.constant)
    ok = false
  case 2:
    res^ = rand.choice(params)
    ok = true
  case 3:
    f1, ok1 := generate_function(params, depth - 1)
    f2, ok2 := generate_function(params, depth - 1)
    ok = ok1 || ok2
    res^ = Add{f1, f2}
  case 4:
    f1, ok1 := generate_function(params, depth - 1)
    f2, ok2 := generate_function(params, depth - 1)
    ok = ok1 || ok2
    res^ = Mul{f1, f2}
  case 5:
    f1, ok1 := generate_function(params, depth - 1)
    ok |= ok1
    res^ = Pow{rand.choice(powers[:]), f1}
  case 6:
    f1, ok1 := generate_function(params, depth - 1)
    ok |= ok1
    res^ = Fun{rand.choice(functions[:]), f1}
  case 7:
    f1, ok1 := generate_function(params, depth - 1)
    f2, ok2 := generate_function(params, depth - 1)
    f3, ok3 := generate_function(params, depth - 1)
    f4, ok4 := generate_function(params, depth - 1)
    ok = (ok1 || ok2) && (ok3 || ok4)
    res^ = If{rand.choice(conditions[:]), f1, f2, f3, f4}
  }
  return
}

compute_function :: proc(fun: ^Expr, params: map[string]f64) -> (res: f64) {
  switch v in fun^ {
  case f64:
    res = v
  case string:
    res = params[v]
  case Add:
    res = compute_function(v.e1, params) + compute_function(v.e2, params)
  case Mul:
    res = compute_function(v.e1, params) * compute_function(v.e2, params)
  case Pow:
    res = math.pow(compute_function(v.e1, params), f64(v.pow))
  case Fun:
    val := compute_function(v.e1, params)
    switch v.typ {
    case .Sin:
      res = math.sin(val)
    case .Abs:
      res = math.abs(val)
    case .Sqrt:
      res = math.abs(val)
    case .Log:
      res = math.abs(val)
    case .Inv:
      res = val == 0 ? val : (1 / val)
    }
  case If:
    val1 := compute_function(v.a, params)
    val2 := compute_function(v.b, params)
    cond: bool
    switch v.cond {
    case .Equal:
      cond = val1 == val2
    case .Less:
      cond = val1 < val2
    case .GreaterEqual:
      cond = val1 >= val2
    }
    if cond {
      res = compute_function(v.c, params)
    } else {
      res = compute_function(v.d, params)
    }
  }
  return
}

Options :: struct {
  filename: string `args:"name=o",usage:"Output filepath"`,
  attempts: int `args:"name=a",usage:"Number of generated images"`,
  constant: f64 `args:"name=c",usage:"Constant size"`,
  size:     f64 `args:"name=s",usage:"Field size"`,
  width:    int `args:"name=width",usage:"Width"`,
  height:   int `args:"name=height",usage:"Height"`,
  single:   bool `args:"name=l",usage:"Single function"`,
  depth:    int `args:"name=d",usage:"Max function depth"`,
  weights:  string `args:"name=w",usage:"Weights"`,
}

convert_weights :: proc(ws: string) -> (res: [7]f64) {
  parts := strings.split(ws, " ")
  defer delete(parts)
  if len(parts) != 7 {
    fmt.eprintln("There should be 7 weights")
    os.exit(1)
  }
  sums: [7]f64
  for i in 0 ..< 7 {
    res[i] = f64(strconv.atoi(parts[i]))
    for j in i ..< 7 {
      sums[j] += res[i]
    }
  }
  for i in 0 ..< 7 {
    res[i] = sums[i] / sums[6]
  }
  return
}

main :: proc() {
  when ODIN_DEBUG {
    track: mem.Tracking_Allocator
    mem.tracking_allocator_init(&track, context.allocator)
    context.allocator = mem.tracking_allocator(&track)
    defer {
      if len(track.allocation_map) > 0 {
        fmt.eprintf("=== %v allocations not freed: ===\n", len(track.allocation_map))
        for _, entry in track.allocation_map {
          fmt.eprintf("- %v bytes @ %v\n", entry.size, entry.location)
        }
      }
      mem.tracking_allocator_destroy(&track)
    }
  }
  opts := Options{"image.png", 0, 10, math.PI, 1024, 1024, false, 10, "1 1 1 1 1 1 1"}
  flags.parse_or_exit(&opts, os.args, .Unix)
  context.user_ptr = &opts
  if opts.attempts == 0 {
    perform(-1, opts)
  } else {
    for i in 0 ..< opts.attempts {
      perform(i, opts)
      fmt.println()
    }
  }
}

perform :: proc(n: int, opts: Options) {
  gen_params := [2]string{"x", "y"}
  ok: bool
  fun_r, fun_g, fun_b: ^Expr
  for {
    if fun_r != nil do free_expr(fun_r)
    fun_r, ok = generate_function(gen_params[:], opts.depth)
    if ok do break
  }
  defer free_expr(fun_r)
  for {
    if fun_g != nil do free_expr(fun_g)
    fun_g, ok = generate_function(gen_params[:], opts.depth)
    if ok do break
  }
  defer free_expr(fun_g)
  for {
    if fun_b != nil do free_expr(fun_b)
    fun_b, ok = generate_function(gen_params[:], opts.depth)
    if ok do break
  }
  defer free_expr(fun_b)
  print_expr(fun_r^)
  print_expr(fun_g^)
  print_expr(fun_b^)
  width := opts.width
  height := opts.height

  buf := make([dynamic]u8, width * height * 3)
  defer delete(buf)

  wg: sync.Wait_Group
  wd_r := WorkerData{&wg, buf[:], width, height, fun_r}
  wd_g := WorkerData{&wg, buf[:], width, height, opts.single ? fun_r : fun_g}
  wd_b := WorkerData{&wg, buf[:], width, height, opts.single ? fun_r : fun_b}

  t_r := thread.create(worker)
  t_r.init_context = context
  t_r.user_index = 0
  t_r.data = &wd_r

  t_g := thread.create(worker)
  t_g.init_context = context
  t_g.user_index = 1
  t_g.data = &wd_g

  t_b := thread.create(worker)
  t_b.init_context = context
  t_b.user_index = 2
  t_b.data = &wd_b

  thread.start(t_r)
  thread.start(t_g)
  thread.start(t_b)

  sync.wait_group_add(&wg, 3)

  thread.destroy(t_r)
  thread.destroy(t_g)
  thread.destroy(t_b)

  fname := opts.filename
  if n >= 0 {
    dir, name := filepath.split(fname)
    fname = filepath.join({dir, fmt.tprintf("%v_%03d%v", filepath.stem(fname), n, filepath.ext(fname))}, context.temp_allocator)
  }
  image.write_png(fmt.ctprint(fname), auto_cast width, auto_cast height, 3, raw_data(buf), auto_cast width * 3)
  free_all(context.temp_allocator)
}

worker :: proc(t: ^thread.Thread) {
  wd: ^WorkerData = auto_cast t.data
  defer sync.wait_group_done(wd.waitgroup)
  width := wd.width
  height := wd.height
  buf_c := make([dynamic]f64, width * height)
  defer delete(buf_c)
  opts: ^Options = auto_cast context.user_ptr
  params := make(map[string]f64)
  defer delete(params)
  for i in 0 ..< width * height {
    x := i % width
    y := i / width
    params["x"] = f64(x) / f64(width) * opts.size * 2 - opts.size
    params["y"] = f64(y) / f64(height) * opts.size * 2 - opts.size
    buf_c[i] = compute_function(wd.fun, params)
  }
  min := buf_c[0]
  max := min
  for i in 1 ..< width * height {
    if buf_c[i] < min do min = buf_c[i]
    if buf_c[i] > max do max = buf_c[i]
  }
  for i in 0 ..< width * height {
    wd.buf[3 * i + t.user_index] = u8((buf_c[i] - min) / (max - min) * 255)
  }
}

WorkerData :: struct {
  waitgroup: ^sync.Wait_Group,
  buf:       []u8,
  width:     int,
  height:    int,
  fun:       ^Expr,
}
