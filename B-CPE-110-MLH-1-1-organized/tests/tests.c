#include <criterion/criterion.h>
#include <stdio.h>
#include "../include/shell.h"

Test(disp, null_data) {
    struct Node* node = NULL;
    int result = disp(&node, NULL);

    cr_assert_eq(result, 0, "Must return 0 if data = NULL");
}

Test(disp, single_node) {
    struct Node node1 = {"Type1", 1, "Node1", NULL};
    struct Node* head = &node1;
    int result = disp(&head, NULL);

    cr_assert_eq(result, 0, "Must return 0 if one node");
}

Test(disp, multiple_nodes) {
    struct Node node1 = {"Type1", 1, "Node1", NULL};
    struct Node node2 = {"Type2", 2, "Node2", NULL};
    struct Node node3 = {"Type3", 3, "Node3", NULL};
    struct Node* head = &node1;
    int result = disp(&head, NULL);

    node1.next = &node2;
    node2.next = &node3;

    cr_assert_eq(result, 0, "Must return 0 if some nodes");
}

Test(del_function, delete_existing_element) {
    struct Node node1 = {"Type1", 1, "Node1", NULL};
    struct Node node2 = {"Type2", 2, "Node2", NULL};
    struct Node node3 = {"Type3", 3, "Node3", NULL};
    struct Node* head = &node1;

    char *args[] = {"1", NULL};
    int result = del(&head, args);
    
    cr_assert_eq(result, 0, "Deletion of existing element failed");
}

Test(del_function, delete_nonexistent_element) {
    struct Node node1 = {"Type1", 1, "Node1", NULL};
    struct Node node2 = {"Type2", 2, "Node2", NULL};
    struct Node node3 = {"Type3", 3, "Node3", NULL};
    struct Node* head = &node1;

    char *args[] = {"100", NULL};
    int result = del(&head, args);
    
    cr_assert_eq(result, 84, "Deletion of non-existent element should return 84");
}