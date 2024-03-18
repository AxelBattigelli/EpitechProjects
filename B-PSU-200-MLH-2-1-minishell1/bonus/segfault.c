int main(void);

int main(void)
{
    *(volatile char *) 0 = 1;
}
