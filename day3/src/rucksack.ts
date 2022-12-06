import * as fs from 'fs';

// splits a string in half
function split_word(str: string): [string, string] {
    const len = str.length;
    return [str.slice(0, len / 2), str.slice(len / 2)];
}

function common_letter(arr: string[]): string {
    const sets = arr.map(str => new Set(str.split('')));
    let intersect = Array.from(sets[0]);

    sets.forEach(set => intersect = intersect.filter(char => set.has(char)));

    return intersect[0];
}

function to_priority(str: string): number {
    const ascii = str.charCodeAt(0);

    if (ascii >= 65 && ascii <= 90) { // uppercase
        return ascii - 64 + 26;
    } else if (ascii >= 97 && ascii <= 122) { // lowercase
        return ascii - 96;
    }

    return 0;
}

function group(arr: string[], size: number): string[][] {
    console.assert(arr.length % size == 0);
    let result: string[][] = [];
    
    for (let i = 0; i < arr.length; i += size) {
        result.push(arr.slice(i, i + size));
    }
    
    return result;
}

// PART 1
const input = fs.readFileSync('input.txt', { encoding: 'utf-8'});
const lines = input.split('\n');
const split_lines = lines.map(split_word);
const common_letters = split_lines.map(common_letter);
const priorities = common_letters.map(to_priority);
const priorities_sum = priorities.reduce((a, b) => a + b);
console.log(priorities_sum);

// PART 2
const grouped = group(lines, 3);
const common_letters_2 = grouped.map(common_letter);
const priorities_2 = common_letters_2.map(to_priority);
const priorities_sum_2 = priorities_2.reduce((a, b) => a + b);
console.log(priorities_sum_2);