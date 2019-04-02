use std::sync::Mutex;

pub struct M<'a> {
    r: &'a [u8],
    m: Mutex<&'a u8>,
}

mod store {
    use super::M;

    #[selfstack::selfstack]
    pub struct Store {
        data: Vec<u8>,
        m: M<'data>,
    }
}

#[test]
fn test() {
    let mut s = store::Store::new();
    let sub = s.build_data(vec![0]);
    let sub = sub.build_m(|data| M {
        r: &data,
        m: Mutex::new(&data[0]),
    });
    assert_eq!(sub.m().r, &[0]);
    assert_eq!(*sub.m().m.lock().unwrap(), &0);
    std::mem::drop(sub);
}
