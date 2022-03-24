from inventory_level_computation import * 

def test_IL_prob_array_discrete_positive():
    # Test cases are (R, Q, demand_probability_array)
    test_case_1 = (0, 5, np.array([0.5,0.5]))
    IL_prob_arr = IL_prob_array_discrete_positive(*test_case_1)
    print(f"Test case 1: {test_case_1}, prob array: {IL_prob_arr}")

    # Test length:
    assert len(IL_prob_arr) == 6, f"Test case 1: The array has length {len(IL_prob_arr)}, it should have length 6"
    # Test type: 
    assert type(IL_prob_arr) is np.ndarray, f"Test case 1: Type is {IL_prob_arr}, should be np.ndarray"
    # Assert correctness
    correct_test_case_1 = np.array([0.1,0.2,0.2,0.2,0.2,0.1])
    assert all(IL_prob_arr == correct_test_case_1), f"Some probabilities are wrong.\n Correct: {correct_test_case_1} \n Produced: {IL_prob_arr}"
    
    test_case_2 = (5, 3, np.array([0.5,0.25,0.25]))
    IL_prob_arr = IL_prob_array_discrete_positive(*test_case_2)
    print(f"Test case 2: {test_case_2}, prob array: {IL_prob_arr}")

    # Test length:
    assert len(IL_prob_arr) == 9, f"Test case 2: The array has length {len(IL_prob_arr)}, it should have length 9"
    # Test type: 
    assert type(IL_prob_arr) is np.ndarray, f"Test case 2: Type is {IL_prob_arr}, should be np.ndarray"
    # Assert correctness
    correct_test_case_2 = np.array([0,0,0,0,1/12,1/6,1/3,3/12,1/6])
    assert all(np.abs(IL_prob_arr-correct_test_case_2)<1e-4 ), f"Some probabilities are wrong.\n Correct: {correct_test_case_2} \n Produced: {IL_prob_arr}"
 

    
   
def main():
    test_IL_prob_array_discrete_positive()

if __name__ == "__main__":
    main()

    

    



