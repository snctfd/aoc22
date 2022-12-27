import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Scanner;
import java.util.Stack;


abstract class DirTreeNode {
    public String name;
    public Dir parent;

    public DirTreeNode(String name, Dir parent) {
        this.name = name;
        this.parent = parent;
    }

    public abstract long getSize();
    public DirTreeNode getChild(String name) { return null; }
    public Iterable<DirTreeNode> getChildren() { return null; }
    public boolean isDir() { return false; }
}

class Dir extends DirTreeNode {
    private Map<String, DirTreeNode> contents = new HashMap<>();

    public Dir(String name, Dir parent) {
        super(name, parent);
    }

    public void addFile(String name, long size) {
        contents.put(name, new File(name, this, size));
    }

    public void addDir(String name) {
        contents.put(name, new Dir(name, this));
    }

    @Override
    public long getSize() {
        long sum = 0;

        for (DirTreeNode child : contents.values()) {
            sum += child.getSize();
        }

        return sum;
    }

    @Override
    public DirTreeNode getChild(String name) {
        return contents.get(name);
    }

    @Override
    public Iterable<DirTreeNode> getChildren() {
        return contents.values();
    }

    @Override
    public boolean isDir() {
        return true;
    }
}

class File extends DirTreeNode {
    public long size = 0;

    public File(String name, Dir parent, long size) {
        super(name, parent);
        this.size = size;
    }

    @Override
    public long getSize() {
        return size;
    }
}

class DirIterator implements Iterator<Dir> {
    private Stack<Dir> stack = new Stack<>();

    DirIterator(Dir start) {
        stack.push(start);
    }

    @Override
    public boolean hasNext() {
        return !stack.isEmpty();
    }

    @Override
    public Dir next() {
        Dir cur = stack.pop();

        for (DirTreeNode child : cur.getChildren()) {
            if (child.isDir()) {
                stack.push((Dir)child);
            }
        }

        return cur;
    }

}

public class DirTree {
    private static Scanner scanner;
    private static Dir root;
    private static Dir cur;

    private static void cd(String dir) {
        switch (dir) {
            case "/":
                cur = root;
                break;
            case "..":
                cur = cur.parent;
                break;
            default:
                cur = (Dir)cur.getChild(dir);
                break;
        }
    }

    private static void ls() {
        while (scanner.hasNext("(dir)|([0-9]+)")) {
            String token = scanner.next();

            if (token.equals("dir")) {
                String name = scanner.next();
                cur.addDir(name);
            } else {
                long size = Long.parseLong(token);
                String name = scanner.next();
                cur.addFile(name, size);
            }
        }
    }

    private static void parseCmd() {
        if (!scanner.next().equals("$")) {
            throw new RuntimeException("command lines should start with $!");
        }

        String cmd = scanner.next();
        switch (cmd) {
            case "cd":
                cd(scanner.next());
                break;
            case "ls":
                ls();
                break;
            default:
                throw new RuntimeException("unknown command: " + cmd);
        }
    }

    public static void main(String[] args) {
        scanner = new Scanner(System.in);
        root = new Dir("/", null);
        cur = root;

        while (scanner.hasNext()) {
            parseCmd();
        }

        scanner.close();

        DirIterator di = new DirIterator(root);
        long freeSize = 70000000L - root.getSize();
        long partOneSize = 0;
        long partTwoSize = Long.MAX_VALUE;
        while (di.hasNext()) {
            Dir d = di.next();
            long size = d.getSize();
            if (size <= 100000) {
                partOneSize += size;
            }

            if (freeSize + size > 30000000L && size < partTwoSize) {
                partTwoSize = size;
            }

        }

        System.out.println("sum of dir sizes <= 100000: " + partOneSize);
        System.out.println("smallest dir size to delete to reach 30000000 free: " + partTwoSize);
    }
}