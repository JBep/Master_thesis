import numpy as np

def find_smallest_divisor(integer_array: np.ndarray):
    """Finds the smalllest common divisor among an array of integers."""

    for i in range(np.max(integer_array),0,-1):
        divisor = i
        smallest = False
        for val in integer_array:
            if val%divisor != 0:
                smallest = False
                break
            smallest = True
        if smallest: break

    return divisor

def main():
    print(f"Smallest divisor is: {find_smallest_divisor([8,8,8,4,4,8])}")
                
if __name__ == "__main__":
    main()