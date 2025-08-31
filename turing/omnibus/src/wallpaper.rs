// wallpaper.rs
// Algorithm for drawing a wallpaper from tuning omnibus

use rand::Rng;
use std::arch::x86_64::_SIDD_CMP_EQUAL_ANY;
use std::io::Write;
use image::{RgbImage, Rgb};
 use std::fs::File;
const WIDTH: u32 = 800;
const HEIGHT: u32 = 600;

pub fn draw_wallpaper(filename: &str) {
    let corna: f64 = 0.0;
    let cornb = 0.0;
    let side = 5.0;
    
    if filename.ends_with(".png") {
        for s in (300..=(WIDTH / 2)).step_by(10) {
            let mut img = RgbImage::new(WIDTH, HEIGHT);
            for i in 0..WIDTH {
            for j in 0..HEIGHT {
                let x = corna + (i as f64) * (s as f64) / HEIGHT as f64;
                let y = cornb + j as f64 * (s as f64) / WIDTH as f64;
                let c = x * x + y * y;
                if c as i32 % 2 == 0 {
                    img.put_pixel(i, j, Rgb([255, 0, 0]));
                } 
                else if c as i32 % 3 == 0 {
                    img.put_pixel(i, j, Rgb([0, 255, 0]));
                }
                else if c as i32 % 4 == 0 {
                    img.put_pixel(i, j, Rgb([0, 0, 255]));
                } else {
                    img.put_pixel(i, j, Rgb([0, 0, 0]));
                }
            }
            }
            let out_filename = format!("side_{}_{}", s, filename);
            img.save(&out_filename).expect("Unable to save PNG file");
        }
        
    }
}
