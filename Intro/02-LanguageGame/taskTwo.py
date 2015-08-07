STR_FOR_WOMAN = 'tta'
STR_FOR_MAN = 'ss'


def get_list_with_names(names):
    people = []
    name = ''
    for elem in names:
        if elem != ' ':
            name += elem
        else:
            people.append(name)
    return people


def get_num_of_woman_and_man(list_with_names):
    woman = 0
    man = 0
    for name in list_with_names:
        if name[(len(name) - 3)::] == STR_FOR_WOMAN:
            woman += 1
        else:
            man += 1
    return (man, woman)


def get_chance():
    input_for_statistic = input()
    input_for_names = input()

    percent_for_man = 0
    percent_for_woman = 0

    names = get_list_with_names(input_for_names)
    man_and_woman = get_num_of_woman_and_man(names)

    know_man = int(input_for_statistic[0])
    know_woman = int(input_for_statistic[2])

    if know_man == 0:
        percent_for_man = 0
    elif max(know_man, man_and_woman[0]) - min(know_man, man_and_woman[0]) < 2:
        percent_for_man = 100
    else:
        percent_for_man = 100/(max(know_man, man_and_woman[0]) - min(know_man, man_and_woman[0]))

    if know_woman == 0:
        percent_for_woman = 0
    elif max(know_woman, man_and_woman[1]) - min(know_woman, man_and_woman[1]) < 2:
        percent_for_woman = 100
    else:
        percent_for_woman = 100/(max(know_woman, man_and_woman[1]) - min(know_woman, man_and_woman[1]))

    return str(int((percent_for_woman + percent_for_man) / 2)) + '%'


def main():
    print(get_chance())


if __name__ == '__main__':
    main()
