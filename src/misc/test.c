
enum SExpType {
    TypeCons,
    TypeAtom
};

enum AtomType {
    Num,
    Str,
    Nil
};

typedef struct {
    enum AtomType type;
    union {
        int num;
        char* str;
    } atom; 
} Atom;

struct SExp;

typedef struct {
    struct SExp* car;
    struct SExp* cdr;
} Cons;


typedef struct {
    enum SExpType type;
    union {
        Cons cons;
        Atom atom;
    } sexp;
} SExp;

typedef struct {
    
} Env;

int main() {
    return 0;
}