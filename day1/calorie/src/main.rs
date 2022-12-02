use std::{fs};

// groups the elements of coll into a new Vec<Vec<T>>, 
// where the inner vectors contain consecutive elements
// for which P returns true, essentially splitting the 
// vector at the elements for which P returns false.
// the elements for which P is false are excluded.
fn group_by<T, P>(coll: Vec<T>, pred: P) -> Vec<Vec<T>>
where P: Fn(&T) -> bool {
    let mut result: Vec<Vec<T>> = Vec::new();
    let mut temp_result: Vec<T> = Vec::new();
    
    for x in coll.into_iter() {
        if pred(&x) {
            temp_result.push(x);
        } else {
            result.push(temp_result);
            temp_result = Vec::new();
        }
    }

    return result;
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file!");
    let lines_vec: Vec<&str> = input.lines().collect();
    let grouped = group_by(lines_vec, |&s| !s.is_empty());
    let mut sums: Vec<i32> =
        grouped.into_iter()
               .map(|xs| 
                        xs.iter()
                          .map(|&s| s.parse::<i32>().expect("PARSE ERROR"))
                          .sum()
                    )
               .collect();
    
    sums.sort_by(|a, b| b.cmp(a)); // descending order
    print!("max calories: {}\n", sums.first().unwrap());
    print!("top 3 calories sum: {}\n", sums.iter().take(3).sum::<i32>());
    
}
