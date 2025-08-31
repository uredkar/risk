mod wallpaper;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        println!("Usage: omnibus <concept> <filename>");
        return;
    }
    match args[1].as_str() {
        "wallpaper" => {
            wallpaper::draw_wallpaper(&args[2]);
            println!("Wallpaper generated: {}", args[2]);
        }
        // Add more concepts here
        _ => {
            println!("Unknown concept: {}", args[1]);
        }
    }
}
