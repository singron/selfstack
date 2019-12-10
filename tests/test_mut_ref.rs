struct A<'a> {
    x: &'a u32,
}

selfstack::selfstack! {
    mod store {
        use super::*;

        pub(super) struct Store<'a> {
            a: A<'a>,
        }
    }
}

#[test]
fn test_mut_ref() {
    let x = 1;
    let y = 2;
    let z = 3;
    let mut s = store::Store::new();
    let mut s_a = s.set_a(A { x: &x });
    {
        s_a.mut_a().x = &y;
    }
    assert_eq!(s_a.ref_a().x, &y);
    {
        let view = s_a.view();
        view.a.x = &z;
    }
    assert_eq!(s_a.ref_a().x, &z);
}
