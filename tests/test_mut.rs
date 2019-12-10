struct A {
    x: u32,
}

selfstack::selfstack! {
    mod store {
        use super::*;
        pub(super) struct Store {
            a: A,
        }
    }
}

#[test]
fn test_mut() {
    let mut s = store::Store::new();
    let mut s_a = s.set_a(A { x: 1 });
    {
        s_a.mut_a().x = 2;
    }
    assert_eq!(s_a.ref_a().x, 2);
    {
        let view = s_a.view();
        view.a.x = 3;
    }
    assert_eq!(s_a.ref_a().x, 3);
}
