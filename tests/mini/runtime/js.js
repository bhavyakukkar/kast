// @ts-nocheck
const Kast = {};

const stdout = { buffer: "", write: console.log };

const print_into = (f, s) => {
  f.buffer += s;
  for (;;) {
    const newline_idx = f.buffer.indexOf("\n");
    if (newline_idx < 0) {
      break;
    }
    f.write(f.buffer.substring(0, newline_idx));
    f.buffer = f.buffer.substring(newline_idx + 1);
  }
};

Kast.print_string = (s) => print_into(stdout, s);
Kast.print_int32 = (x) => print_into(stdout, x.toString());

globalThis.Kast = Kast;
