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

fn mut_a(s_a: &mut store::Store_a) {
    let a = s_a.mut_a();
}

fn test_mut() {
    let mut s = store::Store::new();
    let mut s_a = s.build_a(A { x: 1 });
    {
        mut_a(&mut s_a);
    }
}
