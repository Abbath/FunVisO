package main

import "core:fmt"
import "core:math"
import "core:math/rand"
import "vendor:stb/image"

Add :: struct {
  e1: ^Expr,
  e2: ^Expr,
}

Mul :: struct {
  e1: ^Expr,
  e2: ^Expr,
}

Expr :: union {
  f64,
  string,
  Add,
  Mul,
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
  }
}

generate_function :: proc(params: []string) -> ^Expr {
  variants := [4]int{1, 2, 3, 4}
  n := rand.choice(variants[:])
  e := new(Expr)
  switch n {
  case 1:
    e^ = rand.float64_uniform(-10, 10)
    return e
  case 2:
    e^ = rand.choice(params)
    return e
  case 3:
    e^ = Add{generate_function(params), generate_function(params)}
  case 4:
    e^ = Mul{generate_function(params), generate_function(params)}
  }
  return e
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
  }
  return
}

main :: proc() {
  params := [2]string{"x", "y"}
  fun_r := generate_function(params[:])
  defer free_expr(fun_r)
  fun_g := generate_function(params[:])
  defer free_expr(fun_g)
  fun_b := generate_function(params[:])
  defer free_expr(fun_b)
  fmt.println(fun_r^)
  fmt.println(fun_g^)
  fmt.println(fun_b^)
  width := 1024
  height := 1024
  buf_r := make([dynamic]f64, width * height)
  defer delete(buf_r)
  buf_g := make([dynamic]f64, width * height)
  defer delete(buf_g)
  buf_b := make([dynamic]f64, width * height)
  defer delete(buf_b)
  buf := make([dynamic]u8, width * height * 3)
  defer delete(buf)
  for i in 0 ..< width * height {
    x := i % width
    y := i / width
    params := make(map[string]f64)
    params["x"] = auto_cast x / f64(width) * math.PI * 2 - math.PI
    params["y"] = auto_cast y / f64(height) * math.PI * 2 - math.PI
    r := compute_function(fun_r, params)
    g := compute_function(fun_g, params)
    b := compute_function(fun_b, params)
    buf_r[i] = r
    buf_g[i] = g
    buf_b[i] = b
  }
  min_r := buf_r[0]
  max_r := min_r
  min_g := buf_g[0]
  max_g := min_g
  min_b := buf_b[0]
  max_b := min_b
  for i in 1 ..< width * height {
    if buf_r[i] < min_r do min_r = buf_r[i]
    if buf_r[i] > max_r do max_r = buf_r[i]
    if buf_g[i] < min_g do min_g = buf_g[i]
    if buf_g[i] > max_g do max_g = buf_g[i]
    if buf_b[i] < min_b do min_b = buf_b[i]
    if buf_b[i] > max_b do max_b = buf_b[i]
  }
  for i in 0 ..< width * height {
    buf[3 * i + 0] = u8(buf_r[i] - min_r / (max_r - min_r) * 255)
    buf[3 * i + 1] = u8(buf_g[i] - min_g / (max_g - min_g) * 255)
    buf[3 * i + 2] = u8(buf_b[i] - min_b / (max_b - min_b) * 255)
  }
  image.write_png("image.png", auto_cast width, auto_cast height, 3, raw_data(buf), auto_cast width * 3)
}
