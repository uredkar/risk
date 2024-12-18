mod traits {
    pub mod base;
}
use chrono::prelude::*;
use traits::base::Underlying;

fn main() {
    println!("Hello, world!");
    let u1: Box<Underlying> = Underlying::create("NVDA",Utc::now(),1120.0);
    u1.display();
    let u: Box<Underlying> = Box::new(Underlying {
        symbol: "MSFT".to_string(),
        date: Utc::now(),
        price: 10.0
    });
    u.display();
}
