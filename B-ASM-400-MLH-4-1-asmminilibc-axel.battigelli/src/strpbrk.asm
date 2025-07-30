[BITS 64]

global strpbrk
global my_strpbrk

SECTION .text

strpbrk:
my_strpbrk:
    ENTER 0, 0
    TEST rdi, rdi
    JZ end
    TEST rsi, rsi
    JZ end

next_char:
    MOV al, byte [rdi]
    TEST al, al
    JZ end
    MOV rdx, rsi

check_accept:
    MOV cl, byte [rdx]
    TEST cl, cl
    JZ next_it
    CMP al, cl
    JE return_found
    INC rdx
    JMP check_accept

next_it:
    INC rdi
    JMP next_char

return_found:
    MOV rax, rdi
    LEAVE
    RET

end:
    XOR rax, rax
    LEAVE
    RET
