use crate::v2::ea::{Size::*, EA::*};
use crate::v2::register::{Addr::*, Data::*};
use crate::v2::vm_68k::{Asm, Branch::*, Cond::*};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct HeapFree {
    tag: usize,
    prev: usize,
    size: usize,
    prev_free: usize,
    next_free: usize,
    empty: usize,
}

const HEAP_FREE: HeapFree = HeapFree {
    tag: 0,
    prev: 4,
    size: 8,
    prev_free: 12,
    next_free: 16,
    empty: 20,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct HeapAlloc {
    tag: usize,
    prev: usize,
    size: usize,
    data: usize,
}

const HEAP_ALLOC: HeapAlloc = HeapAlloc {
    tag: 0,
    prev: 4,
    size: 8,
    data: 12,
};

const FREE: i32 = i32::from_be_bytes(*b"free");
const HEAP: i32 = i32::from_be_bytes(*b"heap");
const END: i32 = i32::from_be_bytes(*b"end!");

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Globals {
    heap_root: HeapFree,
}

impl Globals {
    fn init(base: usize) -> Self {
        Self {
            heap_root: HeapFree {
                tag: base,
                prev: base + 4,
                size: base + 8,
                prev_free: base + 12,
                next_free: base + 16,
                empty: base + 20,
            },
        }
    }
}

fn init_heap(asm: &mut Asm, globals: Globals) {
    // in: D0 = heap_size, A0 = heap_addr

    // root.next_free = heap_addr
    asm.mov(Long, Addr(A0), Absolute(globals.heap_root.next_free as i32));

    // build end placeholder (in reverse)
    asm.load_ea(IdxData(A0, D0, 0), Addr(A1));
    // next_free: null
    asm.mov(Long, Immediate(0), PreDec(A1));
    // prev_free: heap base
    asm.mov(Long, Addr(A0), PreDec(A1));
    // size: 0
    asm.mov(Long, Immediate(0), PreDec(A1));
    // prev: heap base
    asm.mov(Long, Addr(A0), PreDec(A1));
    // kind: 'end!'
    asm.mov(Long, Immediate(END), PreDec(A1));
    // A1 now points to beginning of end placeholder block

    // build record
    // kind: free
    asm.mov(Long, Immediate(FREE), PostInc(A0));
    // prev: null
    asm.mov(Long, Immediate(0), PostInc(A0));
    // size: heap_size - allocated header size - end placeholder size
    asm.sub(
        Long,
        Immediate((HEAP_FREE.empty + HEAP_ALLOC.data) as i32),
        Data(D0),
    );
    asm.mov(Long, Data(D0), PostInc(A0));
    // prev_free: root
    asm.mov(Long, Immediate(globals.heap_root.tag as i32), PostInc(A0));
    // next_free: placeholder
    asm.mov(Long, Addr(A1), PostInc(A0));

    asm.ret(0);
}

fn allocate(asm: &mut Asm, globals: Globals) {
    // in: D0 = size
    // out: A0 = pointer to where data will go
    // locals: D1 = new free block size, A1 = ptr to new free block
    let min_alloc = HEAP_FREE.empty - HEAP_ALLOC.data;

    // start at root.next_free
    asm.mov(Long, Absolute(globals.heap_root.next_free as i32), Addr(A0));
    // jump to CMP
    let init_loop = asm.branch(True, Placeholder);

    // go to next free block
    let find_free_block = asm.here();
    asm.mov(Long, Offset(A0, HEAP_FREE.next_free as i16), Addr(A0));

    asm.fixup_branch_to_here(init_loop);

    // check if we're at the end
    asm.cmp(Long, Immediate(END), Addr(A0));
    let check_oom = asm.branch(NotEqual, Placeholder);
    // set A0 to null (TODO: maybe we should just crash now)
    asm.mov(Long, Immediate(0), Addr(A0));
    asm.ret(0);
    asm.fixup_branch_to_here(check_oom);

    // check if data fits
    asm.cmp(Long, Offset(A0, HEAP_FREE.size as i16), Data(D0));
    asm.branch(Greater, Line(find_free_block));

    // update current block header
    asm.mov(Long, Immediate(HEAP), Offset(A0, 0));

    // space available for next free block
    asm.mov(Long, Offset(A0, HEAP_FREE.size as i16), Data(D1));
    asm.sub(Long, Data(D0), Data(D1));
    asm.sub(Long, Immediate(HEAP_ALLOC.data as i32), Data(D1));

    // check if enough space to make free block (min 8 bytes)
    asm.cmp(Long, Immediate(min_alloc as i32), Data(D1));
    let to_else = asm.branch(LessEqual, Placeholder);
    // split block
    {
        // update the size of this block
        asm.mov(Long, Data(D0), Offset(A0, HEAP_ALLOC.size as i16));
        // get addr of new block (current block + alloc data + alloc header size)
        asm.load_ea(IdxData(A0, D0, HEAP_ALLOC.data as u8), Addr(A1));

        // update prev link in block after next (new block + alloc data + alloc header size + prev field offset)
        asm.mov(
            Long,
            Addr(A1),
            IdxData(A1, D1, (HEAP_ALLOC.data as u8) + (HEAP_ALLOC.prev as u8)),
        );

        // save A2
        asm.mov(Long, Addr(A2), Data(D0));
        // update prev free link
        asm.mov(Long, Offset(A0, HEAP_FREE.prev_free as i16), Addr(A2));
        asm.mov(Long, Addr(A1), Offset(A2, HEAP_FREE.next_free as i16));
        // update next free link
        asm.mov(Long, Offset(A0, HEAP_FREE.next_free as i16), Addr(A2));
        asm.mov(Long, Addr(A1), Offset(A2, HEAP_FREE.prev_free as i16));
        // restore A2
        asm.mov(Long, Data(D0), Addr(A2));

        // Write new free block
        // kind: free
        asm.mov(Long, Immediate(FREE), PostInc(A1));
        // prev: block
        asm.mov(Long, Addr(A0), PostInc(A1));
        // size:
        asm.mov(Long, Data(D1), PostInc(A1));
        // prev_free: block.prev_free
        asm.mov(Long, Offset(A0, HEAP_FREE.prev_free as i16), PostInc(A1));
        // next_free: block.next_free
        asm.mov(Long, Offset(A0, HEAP_FREE.next_free as i16), PostInc(A1));

        // update A0 to point to data & return
        asm.add(Long, Immediate(HEAP_ALLOC.data as i32), Addr(A0));
        asm.ret(0);
    };
    // otherwise
    {
        asm.fixup_branch_to_here(to_else);
        // save A2
        asm.mov(Long, Addr(A2), Data(D1));
        // remove from freelist
        asm.mov(Long, Offset(A0, HEAP_FREE.prev_free as i16), Addr(A1));
        asm.mov(Long, Offset(A0, HEAP_FREE.next_free as i16), Addr(A2));
        asm.mov(Long, Addr(A1), Offset(A2, HEAP_FREE.prev_free as i16));
        asm.mov(Long, Addr(A2), Offset(A1, HEAP_FREE.next_free as i16));
        // restore A2
        asm.mov(Long, Data(D1), Addr(A2));

        // update A0 to point to data & return
        asm.add(Long, Immediate(HEAP_ALLOC.data as i32), Addr(A0));
        asm.ret(0);
    }
}

fn free(asm: &mut Asm, globals: Globals) {
    // in: A0 = ptr to free
    // locals: A1

    // Move A0 to block header
    asm.sub(Long, Immediate(HEAP_ALLOC.data as i32), Addr(A0));
    // Check that this is a 'heap' block
    asm.cmp(Long, Immediate(HEAP), Offset(A0, 0));

    // Put block size in D0
    asm.mov(Long, Offset(A0, HEAP_ALLOC.size as i16), Data(D0));

    // Check if prev block is free
    asm.mov(Long, Offset(A0, HEAP_ALLOC.prev as i16), Addr(A1));
    asm.cmp(Long, Immediate(FREE), Offset(A1, 0));
    let skip_join_prev = asm.branch(NotEqual, Placeholder);
    {
        // untag this block
        asm.mov(Long, Immediate(0), Offset(A0, 0));
        // Save A2
        asm.mov(Long, Addr(A2), Data(D1));
        // Check if next block is also free
        asm.load_ea(IdxData(A0, D0, HEAP_ALLOC.data as u8), Addr(A2));
        asm.cmp(Long, Immediate(FREE), Offset(A2, 0));
        let skip_join_both = asm.branch(NotEqual, Placeholder);
        {
            // untag next block
            asm.mov(Long, Immediate(0), Offset(A2, 0));
            // add this block size + next size + 2 headers to prev size
            asm.add(Long, Offset(A2, HEAP_FREE.size as i16), Data(D0));
            asm.add(Long, Immediate(2 * HEAP_ALLOC.data as i32), Data(D0));
            asm.add(Long, Data(D0), Offset(A1, HEAP_FREE.size as i16));
            // next.next.prev = prev
            asm.mov(
                Long,
                Addr(A1),
                IdxData(A1, D0, HEAP_ALLOC.data as u8 + HEAP_ALLOC.prev as u8),
            );
            // remove next from freelist (using A0 & A1)
            asm.mov(Long, Offset(A2, HEAP_FREE.prev_free as i16), Addr(A0));
            asm.mov(Long, Offset(A2, HEAP_FREE.next_free as i16), Addr(A1));
            asm.mov(Long, Addr(A0), Offset(A1, HEAP_FREE.prev_free as i16));
            asm.mov(Long, Addr(A1), Offset(A0, HEAP_FREE.next_free as i16));
            // Restore A2
            asm.mov(Long, Data(D1), Addr(A2));
            // and return
            asm.ret(0);
        }
        // otherwise
        {
            asm.fixup_branch_to_here(skip_join_both);
            // update next.prev to point to joined block
            asm.mov(Long, Addr(A1), Offset(A2, HEAP_ALLOC.prev as i16));
            // Add this block size to prev block size
            asm.add(Long, Data(D0), Offset(A1, HEAP_FREE.size as i16));
            // Restore A2
            asm.mov(Long, Data(D1), Addr(A2));
            // and return
            asm.ret(0);
        }
    }
    asm.fixup_branch_to_here(skip_join_prev);
    // Check if next block is free
    asm.load_ea(IdxData(A0, D0, HEAP_ALLOC.data as u8), Addr(A1));
    asm.cmp(Long, Immediate(FREE), Offset(A1, 0));
    let skip_join_next = asm.branch(NotEqual, Placeholder);
    {
        // update this block tag
        asm.mov(Long, Immediate(FREE), Offset(A0, 0));
        // untag next block
        asm.mov(Long, Immediate(0), Offset(A1, 0));

        // add free size + header size of next block to this one
        asm.add(Long, Offset(A1, HEAP_FREE.size as i16), Data(D0));
        asm.add(Long, Immediate(HEAP_ALLOC.data as i32), Data(D0));
        asm.mov(Long, Data(D0), Offset(A0, HEAP_FREE.size as i16));
        // next.next.prev = self
        asm.mov(
            Long,
            Addr(A0),
            IdxData(A0, D0, HEAP_ALLOC.data as u8 + HEAP_ALLOC.prev as u8),
        );

        // Save A2
        asm.mov(Long, Addr(A2), Data(D1));

        // replace neighbor in free list & link back
        asm.mov(Long, Offset(A1, HEAP_FREE.prev_free as i16), Addr(A2));
        asm.mov(Long, Addr(A0), Offset(A2, HEAP_FREE.next_free as i16));
        asm.mov(Long, Addr(A2), Offset(A0, HEAP_FREE.prev_free as i16));
        // and the other one
        asm.mov(Long, Offset(A1, HEAP_FREE.next_free as i16), Addr(A2));
        asm.mov(Long, Addr(A0), Offset(A2, HEAP_FREE.prev_free as i16));
        asm.mov(Long, Addr(A2), Offset(A0, HEAP_FREE.next_free as i16));
        // Restore A2
        asm.mov(Long, Data(D1), Addr(A2));
        // and return
        asm.ret(0);
    }
    // otherwise
    {
        asm.fixup_branch_to_here(skip_join_next);
        // update tag
        asm.mov(Long, Immediate(FREE), Offset(A0, 0));
        // Get first element of free list
        asm.mov(Long, Absolute(globals.heap_root.next_free as i32), Addr(A1));
        // Get free size (base size - header)
        asm.sub(Long, Immediate(HEAP_ALLOC.data as i32), Data(D0));
        // insert at beginning of free list
        asm.mov(Long, Addr(A0), Absolute(globals.heap_root.next_free as i32));
        asm.mov(Long, Addr(A0), Offset(A1, HEAP_FREE.prev_free as i16));
        asm.mov(
            Long,
            Immediate(globals.heap_root.tag as i32),
            Offset(A0, HEAP_FREE.prev_free as i16),
        );
        asm.mov(Long, Addr(A1), Offset(A0, HEAP_FREE.next_free as i16));

        // and return
        asm.ret(0);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::v2::vm_68k::VM;

    #[test]
    fn smoke_test() {
        let mut asm = Asm::new();

        let mut vm = VM::new(2048);
        // == high memory ==
        // 2048 top
        // 512  Code
        //      Stack
        // 320  Heap end
        // 64   Heap
        // 16   Globals
        // 0    Init vectors
        // == low memory ==

        let globals = Globals::init(16);
        let init_sp = 512;
        let init_pc = 512;
        let heap_base = 64;
        let heap_size = 256;
        vm.load_memory(0, &(init_sp as i32).to_be_bytes());
        vm.load_memory(4, &(init_pc as i32).to_be_bytes());
        vm.reset();

        let to_main = asm.branch(True, Placeholder);
        let init_heap_ = asm.here();
        init_heap(&mut asm, globals);
        let allocate_ = asm.here();
        allocate(&mut asm, globals);
        let free_ = asm.here();
        free(&mut asm, globals);

        asm.fixup_branch_to_here(to_main);

        asm.mov(Long, Immediate(heap_base), Addr(A0));
        asm.mov(Long, Immediate(heap_size), Data(D0));
        asm.branch_sub(Line(init_heap_));

        // check first heap record
        asm.mov(Long, Absolute(heap_base), PreDec(A7));
        asm.mov(Long, Immediate(FREE), PreDec(A7));
        asm.assert_eq();

        // allocate a block & store pointer at A2
        asm.mov(Long, Immediate(64), Data(D0));
        asm.branch_sub(Line(allocate_));
        asm.mov(Long, Addr(A0), Addr(A2));

        // first heap block should now be allocated
        asm.mov(Long, Absolute(heap_base), PreDec(A7));
        asm.mov(Long, Immediate(HEAP), PreDec(A7));
        asm.assert_eq();

        // A2 should point to data in first heap block
        asm.mov(Long, Addr(A2), PreDec(A7));
        asm.mov(
            Long,
            Immediate(heap_base + HEAP_ALLOC.data as i32),
            PreDec(A7),
        );
        asm.assert_eq();

        // First free block should be second block in heap
        asm.mov(
            Long,
            Absolute(globals.heap_root.next_free as i32),
            PreDec(A7),
        );
        asm.load_ea(Absolute(heap_base), Addr(A0));
        asm.mov(Long, Offset(A0, HEAP_ALLOC.size as i16), Data(D0));
        asm.load_ea(IdxData(A0, D0, HEAP_ALLOC.data as u8), PreDec(A7));
        asm.assert_eq();

        // Allocate another block and store pointer at A3
        asm.mov(Long, Immediate(32), Data(D0));
        asm.branch_sub(Line(allocate_));
        asm.mov(Long, Addr(A0), Addr(A3));

        // Free first allocated block
        asm.mov(Long, Addr(A2), Addr(A0));
        asm.branch_sub(Line(free_));

        // First block in heap should now be free
        asm.mov(Long, Absolute(heap_base), PreDec(A7));
        asm.mov(Long, Immediate(FREE), PreDec(A7));
        asm.assert_eq();

        // First block in free list should be this block
        asm.mov(Long, Immediate(heap_base), PreDec(A7));
        asm.mov(
            Long,
            Absolute(globals.heap_root.next_free as i32),
            PreDec(A7),
        );
        asm.assert_eq();

        // Get second block
        asm.mov(Long, Immediate(heap_base), Addr(A0));
        asm.mov(Long, Offset(A0, HEAP_FREE.size as i16), Data(D0));
        asm.load_ea(IdxData(A0, D0, HEAP_ALLOC.data as u8), Addr(A0));

        // Second block should be allocated
        asm.mov(Long, Offset(A0, 0), PreDec(A7));
        asm.mov(Long, Immediate(HEAP), PreDec(A7));
        asm.assert_eq();

        // Get third block
        asm.mov(Long, Offset(A0, HEAP_FREE.size as i16), Data(D0));
        asm.load_ea(IdxData(A0, D0, HEAP_ALLOC.data as u8), Addr(A0));

        // third block should be free
        asm.mov(Long, Offset(A0, 0), PreDec(A7));
        asm.mov(Long, Immediate(FREE), PreDec(A7));
        asm.assert_eq();

        asm.stop();

        vm.load_memory(init_pc as usize, &asm.out);
        vm.run();
    }
}
