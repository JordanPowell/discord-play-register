import re


def extract_remainder_after_fragments(fragments, content):
    content = content.lower()
    for fragment in fragments:
        fragment = fragment.lower()
        try:
            idx = content.index(fragment)
            return content[idx + len(fragment) + 1:]
        except ValueError:
            continue  # or pass?
    return content


def extract_time(content):
    time_specifiers = ["@", "at"]
    time_specifiers_regex = "|".join(time_specifiers)
    r = re.compile(fr"(?:{time_specifiers_regex}) (\d(?::[0-5]\d)*)")
    match = r.search(content.lower())
    if match is not None:
        return match.groups(1)
    return None
