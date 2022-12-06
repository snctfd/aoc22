package main

import (
    "fmt"
    "bufio"
    "regexp"
    "os"
    "strconv"
)

func check(e error) {
    if e != nil {
        panic(e)
    }
}

func read_crate(file *os.File) byte {
    crateBuf := make([]byte, 3)
    file.Read(crateBuf)

    if crateBuf[0] == '[' {
        return crateBuf[1]
    }

    return 0
}

func read_start_crates(file *os.File) [][]byte {
    charBuf := make([]byte, 1)
    stacks := make([][]byte, 0)

    // read first line
    for {
        crate := read_crate(file)
        new_stack := make([]byte, 0)

        if crate != 0 {
            new_stack = append(new_stack, crate)
        }

        stacks = append(stacks, new_stack)

        file.Read(charBuf)
        if charBuf[0] == '\n' {
            break
        }
    }

    // read rest of lines
    for {
        // skip forward 1 byte and check if we read a '1'; if yes, we're done
        file.Seek(1, 1) 
        file.Read(charBuf)
        if charBuf[0] == '1' {
            break
        }

        // if not, we go back to the start of the line and proceed
        file.Seek(-2, 1) 
        cur_stack := 0
        for {
            crate := read_crate(file)

            // found a new crate, prepend
            if crate != 0 {
                stacks[cur_stack] = append([]byte{crate}, stacks[cur_stack]...)
            }

            cur_stack++
            file.Read(charBuf)
            if charBuf[0] == '\n' {
                break
            }
        }
    }

    return stacks
}

func consume_lines(file *os.File, n int) {
    charBuf := make([]byte, 1)
    cnt := 0

    for cnt < n {
        file.Read(charBuf)
        if charBuf[0] == '\n' {
            cnt++
        }
    }
}

// returns n, from, to
func parse_cmd(line string) (int, int, int) {
    re := regexp.MustCompile("move ([0-9]+) from ([0-9]+) to ([0-9]+)")

    results := re.FindAllStringSubmatch(line, -1)[0]
    n, err := strconv.Atoi(results[1])
    check(err)
    from, err := strconv.Atoi(results[2])
    check(err)
    to, err := strconv.Atoi(results[3])
    check(err)

    return n, from - 1, to - 1
}

func part1() {
    file, err := os.Open("input.txt")
    check(err)

    stacks := read_start_crates(file)
    //fmt.Println(stacks)
    consume_lines(file, 2)
    
    scanner := bufio.NewScanner(file)
    scanner.Split(bufio.ScanLines)

    stacks_1 := stacks

    for scanner.Scan() {
        n, from, to := parse_cmd(scanner.Text())
        
        for i := 0; i < n; i++ {
            from_top := stacks_1[from][len(stacks_1[from]) - 1]
            stacks_1[from] = stacks_1[from][:len(stacks_1[from]) - 1]
            stacks_1[to] = append(stacks_1[to], from_top)
        }
    }

    for _, elem := range stacks {
        top := elem[len(elem) - 1]
        fmt.Printf("%c", top)
    }

    fmt.Println("")
    file.Close()
}

func part2() {
    file, err := os.Open("input.txt")
    check(err)

    stacks := read_start_crates(file)
    //fmt.Println(stacks)
    consume_lines(file, 2)
    
    scanner := bufio.NewScanner(file)
    scanner.Split(bufio.ScanLines)

    for scanner.Scan() {
        n, from, to := parse_cmd(scanner.Text())
        
        len_from := len(stacks[from])
        from_top := stacks[from][len_from - n:]
        stacks[from] = stacks[from][:len_from - n]
        stacks[to] = append(stacks[to], from_top...)
    }

    for _, elem := range stacks {
        top := elem[len(elem) - 1]
        fmt.Printf("%c", top)
    }

    fmt.Println("")
    file.Close()
}

func main() {
    part1()
    part2()
}