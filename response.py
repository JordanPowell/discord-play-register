
class Response:
    def __init__(self, channel, *args, **kwargs):
        self.channel = channel
        self.in_response_to_message = kwargs.get('message')

    def send(self):
        raise NotImplementedError

    def __str__(self):
        return self.response

    @property
    def response(self):
        raise NotImplementedError


class ReactionResponse(Response):
    def __init__(self, *args, **kwargs):
        raise NotImplementedError


class TextResponse(Response):
    def __init__(self, channel, response_text, *args, **kwargs):
        self._response_text = response_text
        super().__init__(channel, *args, **kwargs)

    def send(self):
        return self.channel.send(self.response)

    @property
    def response(self):
        return self._response_text
