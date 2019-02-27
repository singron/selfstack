struct A<'a> {
    drop_count: &'a mut u32,
}
struct B<'a> {
    a: &'a A<'a>,
    drop_count: &'a mut u32,
}
struct C<'a> {
    b: &'a B<'a>,
    drop_count: &'a mut u32,
}

impl Drop for A<'_> {
    fn drop(&mut self) {
        *self.drop_count += 1
    }
}
impl Drop for B<'_> {
    fn drop(&mut self) {
        assert_eq!(
            *self.a.drop_count, 0,
            "A was dropped {} times before B",
            *self.a.drop_count
        );
        *self.drop_count += 1
    }
}
impl Drop for C<'_> {
    fn drop(&mut self) {
        assert_eq!(
            *self.b.a.drop_count, 0,
            "A was dropped {} times before C",
            *self.b.a.drop_count
        );
        assert_eq!(
            *self.b.drop_count, 0,
            "B was dropped {} times before C",
            &self.b.drop_count
        );
        *self.drop_count += 1
    }
}

mod store {
    use super::*;
    #[selfstack::selfstack]
    pub(super) struct Store<'x> {
        a: A<'x>,
        b: B<'a>,
        c: C<'b>,
    }
}

use store::Store;

#[test]
fn test_drop_counts() {
    let mut a_drops = 0;
    let mut b_drops = 0;
    let mut c_drops = 0;
    let mut store: Store = Store::new();
    {
        let a = store.build_a(A {
            drop_count: &mut a_drops,
        });
        let b = a.build_b(|a: &A| B {
            a: a,
            drop_count: &mut b_drops,
        });
        let c = b.build_c(|_a: &A, b: &B| C {
            b: b,
            drop_count: &mut c_drops,
        });
        assert_eq!(*c.a().drop_count, 0);
        assert_eq!(*c.b().drop_count, 0);
        assert_eq!(*c.c().drop_count, 0);
        assert_eq!(*c.c().b.drop_count, 0);
        assert_eq!(*c.c().b.a.drop_count, 0);
        std::mem::drop(c);
        assert_eq!(a_drops, 1);
        assert_eq!(b_drops, 1);
        assert_eq!(c_drops, 1);
    }
    a_drops = 0;
    b_drops = 0;
    c_drops = 0;
    {
        let a = store.build_a(A {
            drop_count: &mut a_drops,
        });
        let b = a.build_b(|a: &A| B {
            a: a,
            drop_count: &mut b_drops,
        });
        assert_eq!(*b.a().drop_count, 0);
        assert_eq!(*b.b().drop_count, 0);
        std::mem::drop(b);
        assert_eq!(a_drops, 1);
        assert_eq!(b_drops, 1);
        assert_eq!(c_drops, 0);
    }
    a_drops = 0;
    b_drops = 0;
    c_drops = 0;
    {
        let a = store.build_a(A {
            drop_count: &mut a_drops,
        });
        assert_eq!(*a.a().drop_count, 0);
        std::mem::drop(a);
        assert_eq!(a_drops, 1);
        assert_eq!(b_drops, 0);
        assert_eq!(c_drops, 0);
    }
}
