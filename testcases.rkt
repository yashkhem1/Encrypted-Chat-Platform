#lang racket

(send my-system signup 'arpit 'pass1)
(send my-system signup 'yash 'pass2)
(send my-system signup 'tushar 'pass3)
(send my-system create-chat-session 'arpit 'yash 'ATY)
(send my-system create-chat-session 'tushar 'yash 'ATY)
(send my-system send-message 'arpit 'yash "HELLO BRO" 'ATY)
(send my-system send-message 'yash 'arpit "HEY BROO" 'ATY)
(send my-system send-message 'yash 'tushar "HOW IS LIFE GOING" 'ATY)
(send my-system check-chat 'arpit)
(send my-system check-chat 'tushar)
(send my-system check-chat 'yash)
