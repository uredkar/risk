

use chrono::prelude::*;

#[derive(Debug)]
pub struct Underlying {
    pub symbol: String,
    pub date: DateTime<Utc>,
    pub price: f32
}

impl Underlying {
    pub fn display(&self) {
        println!("Underlying name: {}, value: {} date: {:?} ", self.symbol, self.price,self.date);
    }
    pub fn create(symbol: &str,date: DateTime<Utc>, price: f32) -> Box<Underlying> {
        Box::new(Underlying {
            symbol: symbol.to_string(),
            date: date,
            price: price
        })
    }
}

