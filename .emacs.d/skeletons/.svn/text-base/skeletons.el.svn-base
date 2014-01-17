;;squelette XHTML 1.0
(define-skeleton xhtml-skel "Insert xhtml skeleton" nil
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"fr\" >
\t<head>
\t\t<title>Titre</title>
\t\t<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\" />
\t\t<link rel=\"stylesheet\" media=\"screen\" type=\"text/css\" title=\"Design\" href=\"style.css\" />
\t</head>
<\t<body>
\t\t" _ "
\t</body>
</html>")

;;squelette pour les sockets C
(define-skeleton c-sockets-header-skel "Insert a header for programming in C with sockets under Unix systems but compatible with Microsoft Windows" nil
"#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h> /* close */
#include <netdb.h> /* gethostbyname */
#define INVALID_SOCKET -1
#define SOCKET_ERROR -1
#define closesocket(s) close(s)
typedef int SOCKET;
typedef struct sockaddr_in SOCKADDR_IN;
typedef struct sockaddr SOCKADDR;
typedef struct in_addr IN_ADDR;")