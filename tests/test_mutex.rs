use std::sync::Mutex;

pub struct M<'a> {
    r: &'a [u8],
    m: Mutex<&'a u8>,
}

selfstack::selfstack! {
    mod store {
        use super::M;

        pub struct Store {
            data: Vec<u8>,
            m: M<'data>,
        }
    }
}

#[test]
fn test() {
    let mut s = store::Store::new();
    let sub = s.set_data(vec![0]);
    let sub = sub.build_m(|data| M {
        r: &data,
        m: Mutex::new(&data[0]),
    });
    assert_eq!(sub.ref_m().r, &[0]);
    assert_eq!(*sub.ref_m().m.lock().unwrap(), &0);
    std::mem::drop(sub);
}
