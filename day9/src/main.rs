
use std::{collections::HashSet, str::FromStr, ops, cmp::max, io::{self, BufRead}, fmt::Display};

#[derive(Debug, Clone, Copy)]
struct Dir{dx: i32, dy: i32}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct Pos{x: i32, y: i32}

#[derive(Debug, Clone, Copy)]
struct Move {
    dir: Dir,
    steps: i32,
}

#[derive(Debug, Clone, Copy)]
struct ShortRope {
    head: Pos,
    tail: Pos
}

#[derive(Debug, Clone)]
struct LongRope {
    body: Vec<Pos>
}

#[derive(Debug, PartialEq, Eq)]
struct ParseError;

impl FromStr for Dir {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "R" => Ok(Dir { dx:  1, dy:  0 }),
            "L" => Ok(Dir { dx: -1, dy:  0 }),
            "U" => Ok(Dir { dx:  0, dy:  1 }),
            "D" => Ok(Dir { dx:  0, dy: -1 }),
             _  => Err(ParseError),
        }
    }    
}

impl FromStr for Move {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (dir_str, steps_str) = s.split_once(' ').ok_or(ParseError)?;
        let (dir, steps) = (Dir::from_str(dir_str)?, steps_str.parse::<i32>().map_err(|_e| ParseError)?);

        Ok(Move{ dir: dir, steps: steps })
    }
}

impl Display for Dir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let repr = match (self.dx, self.dy) {
            ( 1,  0) => "R",
            (-1,  0) => "L",
            ( 0,  1) => "U",
            ( 0, -1) => "D",
            _ => "ERR"
        };

        write!(f, "{}", repr)
    }
}

impl Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.dir, self.steps)
    }
}

impl ops::Add<Dir> for Pos {
    type Output = Self;

    fn add(self, rhs: Dir) -> Self::Output {
        Self { 
            x: self.x + rhs.dx,
            y: self.y + rhs.dy 
        }
    }
}

impl ops::AddAssign<Dir> for Pos {
    fn add_assign(&mut self, rhs: Dir) {
        self.x += rhs.dx;
        self.y += rhs.dy;
    }
}

impl Pos {
    fn dist(self, other: Pos) -> i32 {
        max((self.x - other.x).abs(), (self.y - other.y).abs())
    }
}

impl ShortRope {
    fn do_move(&mut self, dir: Dir) {
        let old_head = self.head.clone();
        self.head += dir;

        if self.head.dist(self.tail) >= 2 {
            self.tail = old_head;
        }
    }
}

impl LongRope {
    fn new(len: i32) -> Self {
        let mut body = vec![];

        for _ in 0..len {
            body.push(Pos {x: 0, y: 0});
        }

        LongRope { body: body }
    }

    fn do_move(&mut self, dir: Dir) {
        self.body[0] += dir;

        for i in 1..self.body.len() {
            let cur = self.body[i];
            let prev = self.body[i-1];

            let diff_x = prev.x - cur.x;
            let diff_y = prev.y - cur.y;


            if diff_x.abs() > 1 || diff_y.abs() > 1 {
                self.body[i].x = cur.x + diff_x.signum();
                self.body[i].y = cur.y + diff_y.signum();
            }
        }
    }

    fn head(&self) -> Pos {
        *self.body.first().unwrap()
    }

    fn tail(&self) -> Pos {
        *self.body.last().unwrap()
    }
}

fn main() {
    let mut short_visited = HashSet::new();
    let mut long_visited = HashSet::new();
    let mut short_rope = ShortRope {
        head: Pos {
            x: 0,
            y: 0
        }, 

        tail: Pos {
            x: 0,
            y: 0
        }
    };
    let mut long_rope = LongRope::new(10);
    

    let stdin = io::stdin();
    
    short_visited.insert(short_rope.tail);
    for line in stdin.lock().lines() {
        let mv = Move::from_str(line.unwrap().trim()).unwrap();
        //println!("------ {} ------", mv);

        for _ in 0..mv.steps {
            short_rope.do_move(mv.dir);
            long_rope.do_move(mv.dir);
            short_visited.insert(short_rope.tail);
            long_visited.insert(long_rope.tail());
            //println!("head {} tail {}", rope.head, rope.tail);
        }
    }

    println!("unique positions visited (short): {}", short_visited.len());
    println!("unique positions visited (long): {}", long_visited.len());
}
