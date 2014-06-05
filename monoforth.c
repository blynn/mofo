#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <gmp.h>
#include "blt.h"

#define ANON(_body_) ({void _()_body_ _;})

#define ABORT(_msg_) ({ puts(_msg_); stack_reset(dstack); cpu_run((void *) find_xt("quit")); return; })

#define POPZ ({ if (stack_empty(dstack)) ABORT("stack underflow"); stack_pop(dstack); })

#define PEEPZ ({ if (stack_empty(dstack)) ABORT("stack underflow"); stack_peep(dstack); })

struct defn_s {
  char immediate, compile_only;
  void **cell;
  uint64_t n, vloc;
};
typedef struct defn_s *defn_ptr;

defn_ptr defn_new(char immediate, char compile_only) {
  defn_ptr r = malloc(sizeof(*r));
  r->compile_only = compile_only;
  r->immediate = immediate;
  r->vloc = r->n = 0;
  r->cell = 0;
  return r;
}

void **defn_grow_n(defn_ptr defn, size_t n) {
  defn->cell = realloc(defn->cell, (defn->n += n) * sizeof(void *));
  return &defn->cell[defn->n - 1];
}

void **defn_grow(defn_ptr defn) {
  defn->cell = realloc(defn->cell, ++defn->n * sizeof(void *));
  return &defn->cell[defn->n - 1];
}

mpz_ptr mpz_new() {
  mpz_ptr r = malloc(sizeof(mpz_t));
  mpz_init(r);
  return r;
}

void mpz_free(mpz_ptr p) {
  mpz_clear(p);
  free(p);
}

struct stack_t {
  int i, max, record, is_mpz;
  void **p;
};
typedef struct stack_t *stack_ptr;

stack_ptr stack_new() {
  stack_ptr r = malloc(sizeof(*r));
  r->is_mpz = r->record = r->i = 0, r->max = 8;
  r->p = malloc(sizeof(*r->p) * r->max);
  return r;
}

stack_ptr stack_new_mpz() {
  stack_ptr r = stack_new();
  r->is_mpz = 1;
  return r;
}

void stack_reset(stack_ptr st) { st->i = 0; }

void stack_free(stack_ptr st) {
  if (st->is_mpz) for(int i = 0; i < st->record; i++) mpz_free(st->p[i]);
}

void **stack_grow(stack_ptr st) {
  if (st->i == st->max) st->p = realloc(st->p, sizeof(*st->p) * (st->max *= 2));
  if (st->i++ == st->record) {
    if (st->is_mpz) st->p[st->record] = mpz_new();
    st->record++;
  }
  return st->p + st->i - 1;
}

int   stack_empty(stack_ptr st) { return !st->i; }
void *stack_peep (stack_ptr st) { return st->p[st->i - 1]; }
void *stack_pop  (stack_ptr st) { return st->p[--st->i]; }
void  stack_push (stack_ptr st, void *p) { *stack_grow(st) = p; }
void  stack_push_mpz(stack_ptr st, mpz_ptr v) {
  if (!st->is_mpz) fprintf(stderr, "BUG!\n"), exit(1);
  mpz_set(*stack_grow(st), v);
}

int main(int argc, char **argv) {
  BLT *dict = blt_new();
  stack_ptr dstack = stack_new_mpz();
  stack_ptr ijstack = stack_new_mpz();
  stack_ptr rstack = stack_new();
  int state = 0;
  char *word = 0, *cursor = 0;
  int get_word() {
    if (!*cursor) return 0;
    while (*cursor == ' ') cursor++;  // Skip whitespace.
    word = cursor;
    while (*cursor && *cursor != ' ') cursor++;  // Read word.
    if (*cursor) *cursor++ = 0;
    return *word;
  }
  void get_until(char x) {
    word = cursor;
    while (*cursor && *cursor != x) cursor++;
    if (*cursor) *cursor++ = 0;  // EOL also terminates the search.
  }

  void get_until_quote() { get_until('"'); }

  void **ip = 0;

  void *run_literal[] = {ANON({ stack_push_mpz(dstack, *ip++); })};

  void *run_literal_ui[] = {ANON({ mpz_set_ui(*stack_grow(dstack), (uintptr_t) *ip++); })};

  void cpu_run(void *p) { (*((void (**)(void *)) p))(p); }

  defn_ptr curdef = 0;
  defn_ptr sentinel_def = 0;
  uint64_t here = 0;
  BLT *vmem = blt_new();
  void vmem_record(void *p) {
    char key[17];
    sprintf(key, "%016lx", here);
    blt_put(vmem, key, p);
  }
  void **vmem_fetch(int64_t n) {
    char key[17];
    sprintf(key, "%016lx", n);
    BLT_IT *it = blt_floor(vmem, key);
    uint64_t m = strtol(it->key, 0, 16);
    n -= m;
    defn_ptr defn = it->data;
    return n < defn->n ? defn->cell + n : 0;
  }

  void compile(void *p) { *defn_grow(curdef) = p, here++; }

  void curdef_new(char immediate, char compile_only) {
    curdef = defn_new(immediate, compile_only);
    vmem_record(curdef);
    curdef->vloc = here;
  }

  defn_ptr find_defn(char *word) {
    BLT_IT *it = blt_get(dict, word);
    return it ? it->data : 0;
  }

  void *find_xt(char *word) {
    defn_ptr defn = find_defn(word);
    return defn ? defn->cell : 0;
  }

  int add_upper = 1;
  void add_entry(char *word) {
    blt_put(dict, word, curdef);
    if (add_upper) {
      char *upper = strdup(word);
      for (char *c = upper; *c; c++) *c = toupper(*c);
      blt_put(dict, upper, curdef);
      free(upper);
    }
  }

  void add_dict(char *word, void (*fun)(), char immediate, char compile_only) {
    curdef_new(immediate, compile_only);
    compile(fun);
    add_entry(word);
  }

#define DICT(   _word_,_fun_) add_dict(_word_,ANON(_fun_),0,0)
#define DICT_C( _word_,_fun_) add_dict(_word_,ANON(_fun_),0,1)
#define DICT_I( _word_,_fun_) add_dict(_word_,ANON(_fun_),1,0)
#define DICT_IC(_word_,_fun_) add_dict(_word_,ANON(_fun_),1,1)
#define FIND_DEFN ({ if (!get_word()) ABORT("empty name"); \
    defn_ptr defn = find_defn(word); \
    if (!defn) ABORT("name not found"); defn; })

  // Words I named.
  DICT("usleep", { mpz_ptr z = PEEPZ; usleep(mpz_get_ui(z)); });
  // PUTC behaves as the standard EMIT, while our EMIT supports UTF-8.
  DICT("putc", { putchar(mpz_get_si(POPZ)); });
  DICT("compile-only", {
    if (curdef == sentinel_def) ABORT("no definition");
    curdef->compile_only = 1;
  });
  DICT_C("2rdrop", { ijstack->i -= 2; });
  DICT_C("(jmp)", { ip += (intptr_t) *ip; });
  DICT_C("(jz)", { mpz_ptr z = POPZ; ip += mpz_sgn(z) ? 1 : (intptr_t) *ip ; });

  // More "standard" words.
  DICT("here", { mpz_set_ui(*stack_grow(dstack), here); });

  DICT_C(">r", { stack_push_mpz(ijstack, POPZ); });
  DICT_C("r>", { stack_push_mpz(dstack, stack_pop(ijstack)); });
  DICT_C("r@", { stack_push_mpz(dstack, stack_peep(ijstack)); });
  DICT_C("rclear", { stack_reset(rstack); });
  DICT_C("j", { stack_push_mpz(dstack, ijstack->p[ijstack->i - 3]); });

  void (*refill)();
  DICT("refill", { refill(); });

  DICT("cr", { putchar('\n'); });

  void **base;
  int get_base() {
    int r = (intptr_t) *base;
    if (r < 2 || r > 62) return 10;
    return r;
  }
  DICT(".", {
    mpz_ptr z = POPZ;
    mpz_out_str(stdout, (uintptr_t) *base, z);
    putchar(' ');
  });

  DICT(".r", {
    mpz_ptr z = POPZ;
    int n = mpz_get_si(z);
    char fmt[8];
    // TODO: Number base.
    if (n > 0 && n <= 1024) sprintf(fmt, "%%%dZd", n); else strcpy(fmt, "%Zd");
    gmp_printf(fmt, POPZ);
  });

  DICT(".s", {
    // TODO: Number base.
    printf("<%d> ", dstack->i);
    for (int i = 0; i < dstack->i; i++) {
      mpz_out_str(stdout, (uintptr_t) *base, dstack->p[i]);
      putchar(' ');
    }
  });

  uintptr_t decode_utf8(char *s) {
    uintptr_t count = 0, r = (unsigned char) *s;
    for (int b = 128; r & b; b >>= 1) r &= ~b, count++;
    if (!count) return r;
    while (--count) {
      if (!(*++s & 0x80)) return r;  // Bad UTF-8.
      r = (r << 6) + (*s & 0x3f);
    }
    return r;
  }

  DICT("char", {
    if (!get_word()) ABORT("empty word");
    mpz_set_ui(*stack_grow(dstack), decode_utf8(word));
  });

  DICT_IC("[char]", {
    if (!get_word()) ABORT("empty word");
    compile(run_literal_ui);
    compile((void *) decode_utf8(word));
  });

#define DICT_STACK(_word_,_n_,_fun_) DICT(_word_, { \
    if (dstack->i < _n_) ABORT("stack underflow"); \
    void **p = dstack->p + dstack->i - 1; _fun_ })
  void swap(void **p, void **q) { void *tmp = *p; *p = *q, *q = tmp; }
  DICT("drop", { POPZ; });
  DICT("dup", { stack_push_mpz(dstack, PEEPZ); });
  DICT_STACK( "swap", 2, { swap(p, p-1); });
  DICT_STACK("2swap", 4, { swap(p, p-2); swap(p-1, p-3); });
  DICT_STACK(  "nip", 2, { swap(p, p-1); dstack->i--; });
  DICT_STACK( "over", 2, { mpz_set(*stack_grow(dstack), *(p-1)); });
  DICT_STACK(  "rot", 3, { swap(p, p-2); swap(p-1, p-2); });
  DICT_STACK( "-rot", 3, { swap(p-1, p-2); swap(p, p-2); });

#define DICT_MPZ_DIRECT(_op_, _mpz_fun_) DICT(_op_, { \
    mpz_ptr x = POPZ; mpz_ptr z = PEEPZ; _mpz_fun_(z, z, x); });
#define DICT_MPZ(_op_, _fun_) DICT(_op_, { \
    mpz_ptr x = POPZ; mpz_ptr z = PEEPZ; _fun_})
  DICT_MPZ_DIRECT("+", mpz_add);
  DICT_MPZ_DIRECT("-", mpz_sub);
  DICT_MPZ_DIRECT("*", mpz_mul);
  DICT_MPZ_DIRECT("/", mpz_fdiv_q);
  DICT_MPZ_DIRECT("mod", mpz_fdiv_r);
  DICT_MPZ_DIRECT("and", mpz_and);
  DICT_MPZ_DIRECT("or", mpz_ior);
  DICT_MPZ_DIRECT("xor", mpz_xor);

  DICT_MPZ("<",   { mpz_set_si(z, -(mpz_cmp(z, x) < 0)); });
  DICT_MPZ(">",   { mpz_set_si(z, -(mpz_cmp(z, x) > 0)); });
  DICT_MPZ("=",   { mpz_set_si(z, -!mpz_cmp(z, x)); });
  DICT_MPZ("<>",  { mpz_set_si(z, -!!mpz_cmp(z, x)); });
  DICT_MPZ("min", { mpz_set(z, mpz_cmp(z, x) < 0 ? z : x); });
  DICT_MPZ("max", { mpz_set(z, mpz_cmp(z, x) > 0 ? z : x); });
  DICT_MPZ("lshift", { mpz_mul_2exp(z, z, mpz_get_ui(x)); });
  DICT_MPZ("rshift", { mpz_fdiv_q_2exp(z, z, mpz_get_ui(x)); });

  DICT("invert", { mpz_ptr z = PEEPZ; mpz_com(z, z); });
  DICT("negate", { mpz_ptr z = PEEPZ; mpz_neg(z, z); });
  DICT("abs",    { mpz_ptr z = PEEPZ; mpz_abs(z, z); });

  DICT_IC("[", { state = 0; });
  DICT("]", { state = 1; });
  DICT("state", { stack_grow(dstack); mpz_set_ui(PEEPZ, state); });

  void codeword_colon(void **p) {
    stack_push(rstack, ip);
    ip = p + 1;
  }
  DICT(":", {
    if (!get_word()) ABORT("empty name");
    curdef_new(0, 0);
    add_entry(word);
    state = 1;
    compile(codeword_colon);
  });

  void *run_exit[] = {ANON({ ip = stack_pop(rstack); })};
  DICT_IC("exit", { compile(run_exit); });
  DICT_IC(";", { compile(run_exit); state = 0; });

  DICT_IC("literal", {
    compile(run_literal);
    mpz_ptr z = mpz_new();
    mpz_init_set(z, POPZ);
    compile(z);
  });

  DICT("immediate", {
    if (curdef == sentinel_def) ABORT("no definition");
    curdef->immediate = 1;
  });

  void *run_compile[] = {ANON({ compile(*ip++); })};
  DICT_IC("postpone", {
    defn_ptr defn = FIND_DEFN;
    if (defn->immediate) {
      compile(defn->cell);
    } else {
      compile(run_compile);
      compile(defn->cell);
    }
  });

  DICT("'", { mpz_set_ui(*stack_grow(dstack), FIND_DEFN->vloc); });

  DICT_IC("[']", {
    compile(run_literal_ui);
    compile((void *) FIND_DEFN->vloc);
  });

  DICT("execute", {
    uint64_t n = mpz_get_ui(POPZ);
    cpu_run((void *) vmem_fetch(n));
  });

  void codeword_create(void **p) {
    mpz_set_ui(*stack_grow(dstack), (uintptr_t) p[1]);
  }

  void codeword_does(void **p) {
    mpz_set_ui(*stack_grow(dstack), (uintptr_t) p[1]);
    stack_push(rstack, ip);
    ip = p[2];
  }

  DICT("create", {
    if (!get_word()) ABORT("empty name");
    curdef_new(0, 0);
    add_entry(word);
    compile(codeword_create);
    compile((void *) (here + 2));
    compile(0);
  });

  DICT_C("does>", {
    curdef->cell[0] = codeword_does;
    curdef->cell[2] = ip;
    ip = stack_pop(rstack);
  });

  DICT("allot", {
    mpz_ptr z = POPZ;
    uintptr_t n = mpz_get_ui(z);
    defn_grow_n(curdef, n);
    here += n;
  });

  // TODO: Switch to mpz for sufficiently large integers.
  DICT(",", {
    mpz_ptr z = POPZ;
    intptr_t n = mpz_get_si(z);
    compile((void *) n);
  });

  DICT("@", {
    mpz_ptr z = PEEPZ;
    uint64_t n = mpz_get_ui(z);
    void **p = vmem_fetch(n);
    if (!p) ABORT("bad address");
    mpz_set_si(z, (intptr_t) *p);
  });

  DICT("!", {
    mpz_ptr addr = POPZ;
    uint64_t n = mpz_get_ui(addr);
    void **p = vmem_fetch(n);
    if (!p) ABORT("bad address");
    mpz_ptr x = POPZ;
    n = mpz_get_ui(x);
    *p = (void *) n;
  });

  uint64_t base_addr;
  DICT("base", { mpz_set_ui(*stack_grow(dstack), base_addr); });

  DICT_I("(", { get_until(')'); });
  DICT_I("\\", { get_until(0); });

  void *run_print[] = {ANON({ fputs((const char *) *ip++, stdout); })};

  DICT_IC(".\"", {
    get_until_quote();
    compile(run_print);
    compile(strdup(word));
  });

  DICT_IC("s\"", {
    get_until_quote();
    uintptr_t len = strlen(word);
    compile(run_literal_ui);
    compile((void *) (here + 5));
    compile(run_literal_ui);
    compile((void *) len);
    compile(find_xt("(jmp)"));
    compile((void *) (len + 1));
    for (unsigned char *c = (unsigned char *) word; *c; c++) {
      compile((void *) (uintptr_t) *c);
    }
  });

  int bye = 0;

  DICT("bye", { bye = 1; });

  char *(*liner)() = ({char*_() {
    char *r = readline("");
    if (r && *r) add_history(r);
    return r;
  }_;});
  // Disable readline for non-interactive sessions.
  if (!isatty(STDIN_FILENO)) liner = ({char*_(){
    char *r = 0;
    size_t n;
    if (-1 == getline(&r, &n, stdin)) {
      free(r);
      r = 0;
    } else {
      char *c = r + strlen(r) - 1;
      if (*c == '\n') *c = 0;
    }
    return r;
  }_;});

  void *run_recurse[] = {ANON({
    defn_ptr defn = *ip++;
    cpu_run(defn->cell);
  })};
  void interpret_word() {
    defn_ptr defn = find_defn(word);
    if (defn) {
      if (state == 1) {
        if (defn->immediate) {
          cpu_run(defn->cell);
        } else {
          if (defn == curdef) {  // Recursion.
            compile(run_recurse);
            compile(defn);
          } else {
            compile(defn->cell);
          }
        }
      } else {
        if (defn->compile_only) ABORT("compile only");
        cpu_run(defn->cell);
      }
    } else {
      if (mpz_set_str(*stack_grow(dstack), word, get_base())) {
        printf(" ['%s'] ", word); ABORT("bad word");
      }
      if (state == 1) {
        mpz_ptr z = mpz_new();
        mpz_set(z, stack_pop(dstack));
        compile(run_literal);
        compile(z);
      }
    }
  }

  curdef_new(0, 0);
  base_addr = here;
  compile((void *) 10);
  base = vmem_fetch(base_addr);

  curdef_new(0, 0);
  add_entry("interpret");
  compile(codeword_colon);
  compile((void*[]){ANON({ mpz_set_si(*stack_grow(dstack), -!!get_word()); })});
  compile(find_xt("(jz)"));
  compile((void *) 4);
  compile((void*[]){interpret_word});
  compile(find_xt("(jmp)"));
  compile((void *) -5);
  compile(run_exit);

  void *init_program[] = {
    find_xt("refill"),
    find_xt("(jz)"),
    (void *) 4,
    find_xt("interpret"),
    find_xt("(jmp)"),
    (void *) -5,
    0,  // Should be "quit", but this is defined by a preset.
  };
  ip = init_program;

  char *tib = 0;
  void refill_terminal() {
    free(tib);
    cursor = tib = liner();
    mpz_set_si(*stack_grow(dstack), -!!tib);
  }

  void refill_files() {
    static int i = 1;
    static FILE *fp = 0;
    for (;;) {
      if (i == argc) {
        refill = refill_terminal;
        mpz_set_si(*stack_grow(dstack), 0);
        return;
      }
      if (!fp) {
        fp = fopen(argv[i], "rb");
        if (!fp) fprintf(stderr, "error opening '%s'\n", argv[i]), exit(1);
      }
      free(tib);
      tib = 0;
      size_t n;
      if (-1 == getline(&tib, &n, fp)) {
        if (!feof(fp)) fprintf(stderr, "read error: '%s'\n", argv[i]), exit(1);
        fclose(fp);
        fp = 0;
        i++;
      } else break;
    }
    char *c = tib + strlen(tib) - 1;
    if (*c == '\n') *c = 0;
    cursor = tib;
    mpz_set_si(*stack_grow(dstack), -1);
  }

  // Load presets.
  void refill_presets() {
    static char *preset[] = {
": 1- 1 - ;",
": 1+ 1 + ;",
": 2* 2 * ;",
": 2/ 2 / ;",
": 0> 0 > ;",
": 0= 0 = ;",
": 0< 0 < ;",
": 0<> 0 <> ;",
": */ >r * r> / ;",
": 2>r swap >r >r ;",
": 2dup over over ;",
": 2drop drop drop ;",
": ? @ . ;",
": +! dup @ rot + swap ! ;",
": if postpone (jz) here 0 , ; immediate compile-only",
": else postpone (jmp) here 0 , swap dup here swap - swap ! ; immediate compile-only",
": then dup here swap - swap ! ; immediate compile-only",
": begin here ; immediate compile-only",
": again postpone (jmp) here - , ; immediate compile-only",
": until postpone (jz) here - , ; immediate compile-only",
": while postpone (jz) here 0 , swap ; immediate compile-only",
": repeat postpone (jmp) here - , dup here swap - swap ! ; immediate compile-only",
": do postpone 2>r 0 here 2>r ; immediate compile-only",
": (loop-check) r> 1+ dup r@ = if drop r> drop -1 else >r 0 then ;",
": (resolve-leaves) r> dup if 2dup dup -rot - swap ! (resolve-leaves) else 2drop then ; "
": loop postpone (loop-check) postpone (jz) r> here - , here (resolve-leaves) ; immediate compile-only",
": leave postpone 2rdrop postpone (jmp) here r> 2>r 0 , ; immediate compile-only",
": (+loop-check) r@ 2dup + 2* r> 2* r@ 2* 1- dup -rot - -rot - * 0< if 2drop r> drop -1 else + >r 0 then ;",
": +loop postpone (+loop-check) postpone (jz) r> here - , here (resolve-leaves) ; immediate compile-only",
": i r@ ; compile-only",
": ?dup dup if dup then ;",
": cell+ 1+ ;",
": cells 1 * ;",
": constant create , does> @ ;",
": variable create 0 , ;",
": decimal 10 base ! ;",
": hex 16 base ! ;",
"hex "
": u6b 6 * rshift 3f and 80 or putc ; "
": emit "
"  dup     80 < if putc else "
"  dup    800 < if dup 1 6 * rshift 0c0 or putc     0 u6b                 else "
"  dup  10000 < if dup 2 6 * rshift 0e0 or putc dup 1 u6b     0 u6b       else "
"  dup 110000 < if dup 3 6 * rshift 0f0 or putc dup 2 u6b dup 1 u6b 0 u6b "
"  then then then then ; "
"decimal ",
": type 0 do dup @ emit 1+ loop ;",
": space 32 emit ;",
": spaces dup 0> if 0 do space loop then ;",
"32 constant bl",
"-1 constant true",
"0 constant false",
": quit rclear refill if interpret state space if .\" compile\" else .\" ok\" then cr quit then bye ;",
": value constant ;",
": to ' 3 + state if postpone literal postpone ! else ! then ; immediate",
": erase dup if 1- swap 0 over ! 1+ swap erase else 2drop then ;",
": c@ @ ;",
": c! ! ;",
": c, , ;",
": u.r .r ;",
": u. . ;",
    0 };
    static char **p = preset;
    if (!*p) {
      init_program[6] = find_xt("quit");
      add_upper = 0;
      refill = refill_files;
      refill();
      return;
    }
    free(tib);
    cursor = tib = strdup(*p++);
    mpz_set_si(*stack_grow(dstack), -1);
  }
  refill = refill_presets;

  curdef_new(0, 0);
  sentinel_def = curdef;
  while (!bye) cpu_run(*ip++);

  // Clean up. TODO: Lots!
  free(tib);
  stack_free(dstack);
  stack_free(ijstack);
  stack_free(rstack);
  return 0;
}
