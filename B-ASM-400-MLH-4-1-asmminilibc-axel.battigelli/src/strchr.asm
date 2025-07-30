[BITS 64]

global strchr
global my_strchr
global index
global my_index

SECTION .text

index:
my_index:
strchr:
my_strchr:
    ENTER 0, 0
    XOR rcx, rcx

loop:
    CMP BYTE [rdi + rcx], sil
    JZ end
    CMP BYTE [rdi + rcx], 0
    JZ not_found
    INC rcx
    JMP loop

end:
    LEA rax, BYTE [rdi + rcx]
    LEAVE
    RET

not_found:
    XOR rax, rax
    LEAVE
    RET
