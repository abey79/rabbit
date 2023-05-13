const XMIN: f64 = -2.0;
const XMAX: f64 = 1.0;
const YMIN: f64 = -1.5;
const YMAX: f64 = 1.5;
const WIDTH: f64 = 80.0;
const HEIGHT: f64 = 40.0;
const THRESHOLD: i32 = 1000;

fn main() {
    let dx = (XMAX - XMIN) / WIDTH;
    let dy = (YMAX - YMIN) / HEIGHT;
    let mut y = YMAX;

    let mut x: f64;
    let mut _x: f64;
    let mut _y: f64;
    let mut xtemp: f64;

    let mut n: i32;

    let mut in_mandel: bool;

    while y >= YMIN {
        x = XMIN;
        while x < XMAX {
            _x = 0.0;
            _y = 0.0;
            n = THRESHOLD;
            in_mandel = true;
            while n > 0 {
                xtemp = _x * _x - _y * _y + x;
                _y = 2.0 * _x * _y + y;
                _x = xtemp;
                n = n - 1;
                if (_x * _x + _y * _y) > 4.0 {
                    in_mandel = false;
                    n = 0;
                }
            }
            if in_mandel {
                print!("*");
            } else {
                print!(".");
            }
            x = x + dx;
        }
        println!();

        y = y - dy;
    }
}
