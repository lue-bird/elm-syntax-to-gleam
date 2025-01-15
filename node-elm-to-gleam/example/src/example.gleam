// import elm
import gleam/erlang
import gleam/io

pub fn main() {
  let input = erlang_read_all()
  io.print(input)
}

fn erlang_read_all() {
  case erlang.get_line("") {
    Ok(input) -> {
      input <> erlang_read_all()
    }
    Error(_) -> {
      ""
    }
  }
}
