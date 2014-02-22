void foo(void) {
  return;
}

int main() {
    void (*p1_foo)();
    void (*p2_foo)();
    void (*p3_foo)();
    void (*p4_foo)();
    void (*p5_foo)();
    void (*p6_foo)();
    void (*p7_foo)();

    p1_foo = foo;
    p2_foo = *foo;
    p3_foo = &foo;
    p4_foo = *&foo;
    p5_foo = &*foo;
    p6_foo = **foo;
    p7_foo = **********************foo;

    (*p1_foo)();
    (*p2_foo)();
    (*p3_foo)();
    (*p4_foo)();
    (*p5_foo)();
    (*p6_foo)();
    (*p7_foo)();

    return 0;
}
