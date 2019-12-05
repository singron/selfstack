struct A {
    x: u32,
}

mod store {
    use super::*;
    #[selfstack::selfstack]
    pub(super) struct Store {
        a: A,
    }
}

#[test]
fn test_mut() {
    let mut s = store::Store::new();
    let mut s_a = s.build_a(A { x: 1 });
    {
        s_a.mut_a().x = 2;
    }
    assert_eq!(s_a.a().x, 2);
    {
        let view = s_a.view();
        view.a.x = 3;
    }
    assert_eq!(s_a.a().x, 3);
}