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
