[BITS 64]

global strrchr
global my_strrchr
global rindex
global my_rindex

SECTION .text

rindex:
my_rindex:
strrchr:
my_strrchr:
    ENTER 0, 0
    XOR rcx, rcx
    MOV r8, -1

loop:
    CMP BYTE [rdi + rcx], sil
    CMOVE r8, rcx
    CMP BYTE [rdi + rcx], 0
    JZ end
    INC rcx
    JMP loop

end:
    CMP r8, -1
    JE not_found
    LEA rax, [rdi + r8]
    LEAVE
    RET

not_found:
    XOR rax, rax
    LEAVE
    RET
